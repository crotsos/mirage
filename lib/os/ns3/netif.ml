(*
 * Copyright (c) 2010 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt
open Printf
open Gc

type id = string

type t = {
  id: id;
  fd: (int * Io_page.t) Lwt_stream.t;
  fd_push : ((int * Io_page.t) option -> unit);
  read_block: unit Lwt_condition.t;
  mutable active: bool;
  mac: string;
}

external pkt_write: string -> int -> Io_page.t -> int -> int -> unit = "caml_pkt_write"
external queue_check: string -> int -> bool = "caml_queue_check"
external register_check_queue: string -> int -> unit =
  "caml_register_check_queue"
exception Ethif_closed

let devices = Hashtbl.create 1

let ethernet_mac_to_string x =
    let chri i = Char.code x.[i] in
    Printf.sprintf "%02x:%02x:%02x:%02x:%02x:%02x"
       (chri 0) (chri 1) (chri 2) (chri 3) (chri 4) (chri 5)

let plug node_name id mac =
 let active = true in
 let (fd, fd_push) = Lwt_stream.create () in
 let read_block = Lwt_condition.create () in
 let t = { id=(string_of_int id); fd; fd_push;
           active; read_block; mac } in
 let _ = 
   if (Hashtbl.mem devices node_name) then (
     let devs = Hashtbl.find devices node_name in 
       Hashtbl.replace devices node_name (devs @ [t])
   ) else (
     Hashtbl.replace devices node_name [t]
   )
 in
   printf "Netif: plug %s.%d\n%!" node_name id;
   return t

let _ = Callback.register "plug_dev" plug
let _ = Callback.register "get_frame" Io_page.get

let demux_pkt node_name dev_id frame = 
(*   Lwt.wakeup_paused (); *)
  try_lwt
    Gc.compact();   
    let devs = Hashtbl.find devices node_name in 
      Lwt_list.iter_p
      (fun dev -> 
        if (dev.id = (string_of_int dev_id)) then
          let pkt = Io_page.get () in 
          let pkt_len = (String.length frame) in
          let _ = (Cstruct.set_buffer frame 0 pkt 0 pkt_len) in
          return (dev.fd_push (Some((pkt_len, pkt))))
        else return ()
      ) devs 
(*       Printf.printf "packet_demux 7\n%!" *)
  with 
  | Not_found ->
    return (Printf.printf "Packet cannot be processed for node %s\n" node_name)
  | ex ->
    return (printf "Error %s\n" (Printexc.to_string ex))
let _ = Callback.register "demux_pkt" demux_pkt


let unplug node_name id =
  try
    let devs = Hashtbl.find devices node_name in
      List.iter ( 
        fun t ->
          if (t.id = id) then
            t.active <- false
      ) devs;
      let new_devs = List.filter (fun t -> t.id <> id) devs in
        Hashtbl.replace devices node_name new_devs;
        printf "Netif: unplug %s.%s\n%!" node_name id
(*     Hashtbl.remove devices id *)
  with Not_found -> ()

let create fn =
  let Some(name) = Lwt.get Topology.node_name in 
    try_lwt
      let devs = Hashtbl.find devices name in
      Lwt_list.iter_p (
        fun t -> 
          let user = fn t.id t in
          let th,_ = Lwt.task () in
            Lwt.on_cancel th (fun _ -> unplug name t.id);
            th <?> user) devs 
    with Not_found ->
      return ()

(* Input a frame, and block if nothing is available *)
let rec input t =
(*   let Some(node_name) = (Lwt.get Topology.node_name) in *)
(*   Gc.compact() ; *)
  lwt Some((len, page)) = Lwt_stream.get t.fd in
(*   Printf.printf "Read a packet from thread %s\n%!" node_name; *)
    return (page)

(* Get write buffer for Netif output *)
let get_writebuf t =
  let page = Io_page.get () in
  (* TODO: record statistics for requesting thread here (in debug mode?) *)
  return page

(* Loop and listen for packets permanently *)
let rec listen t fn =
  match t.active with
  |true ->
    lwt frame = input t in
    Lwt.ignore_result (
      try_lwt 
        fn frame
      with exn ->
        return (printf "EXN: %s bt: %s\n%!" (Printexc.to_string exn) (Printexc.get_backtrace()))
    );
    listen t fn
  |false ->
    return ()

(* Shutdown a netfront *)
let destroy nf =
  printf "tap_destroy\n%!";
  return ()

let unblock_device name ix = 
  try_lwt
    let devs = Hashtbl.find devices name in 
      Lwt_list.iter_p
      (fun dev -> 
        if (dev.id = (string_of_int ix)) then
          return (Lwt_condition.broadcast dev.read_block ())
        else return ()
      ) devs 
  with Not_found ->
    return (Printf.printf "Packet cannot be processed for node %s\n" name)

let _ = Callback.register "unblock_device" unblock_device

(* Transmit a packet from an Io_page *)
let write t page =
  let off = Cstruct.base_offset page in
  let len = Cstruct.len page in
  let Some(node_name) = Lwt.get Topology.node_name in 
  let rec wait_for_queue t = 
    match (queue_check node_name (int_of_string t.id)) with
    | true -> return ()
    | false -> 
      let _ = register_check_queue node_name (int_of_string t.id) in
      lwt _ = Lwt_condition.wait t.read_block in
        wait_for_queue t
    in
  lwt _ = wait_for_queue t in
  let _ = pkt_write node_name (int_of_string t.id) page off len in 
    return ()


(* TODO use writev: but do a copy for now *)
let writev t pages =
  match pages with
  |[] -> return ()
  |[page] -> write t page
  |pages ->
    let page = Io_page.get () in
    let off = ref 0 in
    List.iter (fun p ->
      let len = Cstruct.len p in
      Cstruct.blit_buffer p 0 page !off len;
      off := !off + len;
    ) pages;
    let v = Cstruct.sub page 0 !off in
    write t v
  
let ethid t = 
  t.id

let mac t =
  t.mac 

