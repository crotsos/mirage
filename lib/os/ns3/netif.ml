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

type id = string

type t = {
  id: id;
  fd: Io_page.t Lwt_stream.t;
  fd_push : (Io_page.t option -> unit);
  mutable active: bool;
  mac: string;
}

exception Ethif_closed

let devices = Hashtbl.create 1

let ethernet_mac_to_string x =
    let chri i = Char.code x.[i] in
    Printf.sprintf "%02x:%02x:%02x:%02x:%02x:%02x"
       (chri 0) (chri 1) (chri 2) (chri 3) (chri 4) (chri 5)

let plug node_name id mac =
 let active = true in
 let (fd, fd_push) = Lwt_stream.create () in
 let t = { id=(string_of_int id); fd; fd_push;
           active; mac } in
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
  lwt Some(page) = Lwt_stream.get t.fd in 
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

(* Transmit a packet from an Io_page *)
let write t page =
  let off = Cstruct.base_offset page in
  let len = Cstruct.len page in
(*   lwt len' = Socket.fdbind Activations.write (fun fd -> Socket.write fd page
 *   off len) t.dev in *)
  let len' = 0 in 
  if len' <> len then
    raise_lwt (Failure (sprintf "tap: partial write (%d, expected %d)" len' len))
  else
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

