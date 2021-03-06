(*
 * Copyright (c) 2012 Charalmpos Rotsos <cr409@cl.cam.ac.uk>
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
open OS
open Net
open Printf

module OP = Openflow.Ofpacket
module OC = Openflow.Controller
module OE = OC.Event

let port = 55555
let use_dhcp = false

let pp = Printf.printf

(*********************************************************
 * Host configuration code and traffic generation code 
 *********************************************************)

let rec echo dst chan = 
  try_lwt
    lwt _ =
      while_lwt true do
        lwt buf = Channel.read_some chan in
(*
         return (Printf.printf "%f: read %d\n%!" 
         (Clock.time ())
         (Cstruct.len buf)) 
 *)
          return () 
      done
    in
      return ()
  with Nettypes.Closed -> return (Log.info "Echo" "closed!")

let rec echo_client chan = 
  try_lwt
    let data = String.create 1460 in 
    let rec send_data () = 
        let _ = Channel.write_string chan data 0 (String.length data) in
(*         Printf.printf "%f: Writing new buffer....\n%!" (Clock.time ());   *)
        lwt _ = Channel.flush chan in
          send_data ()
    in
      send_data ()
  with 
    | Nettypes.Closed -> return (Log.info "Echo" "closed!")
    | ex ->  return (Printf.printf "Eroor:%s\n%!" (Printexc.to_string ex))

let rec echo_udp dst buf = 
  return (Printf.printf "%f: read %d\n%!" 
    (Clock.time ())
    (Cstruct.len buf))
  
let rec echo_client_udp mgr dst =
  try_lwt
    let data = Io_page.create 1460 in
    let rec send_data () = 
        lwt _ = Datagram.UDPv4.send mgr dst data in
        lwt () = Time.sleep 1.0 in
          Printf.printf "%f: Writing new buffer....\n%!" (Clock.time ()); 
          send_data ()
    in
      send_data ()
  with 
    | Nettypes.Closed -> return (Log.info "Echo" "closed!")
    | ex ->  return (Printf.printf "Eroor:%s\n%!" (Printexc.to_string ex))

let ip node_id = 
  Nettypes.(
    (ipv4_addr_of_tuple (10l,0l,1l,(Int32.of_int node_id)),
    ipv4_addr_of_tuple (255l,255l,255l,0l),
    [ ipv4_addr_of_tuple (10l,0l,1l,1l) ]
    )) 
      
let print_time () = 
  while_lwt true do
    OS.Time.sleep 1.0 >>
      return ( printf "%03.6f: running process.." (OS.Clock.time ()) )
  done

(* Code to run on the end node *)
let host_inner host_id () =
  printf "%f: running host %d\n%!" (Clock.time ()) host_id;
  let config_host host_id =
    try_lwt 
      Manager.create (fun mgr interface id ->
        lwt _ = Time.sleep 0.1 in 
        match host_id with
        | 1 ->
          lwt _ = Manager.configure interface (`IPv4 (ip host_id)) in
          Printf.printf "%f: trying to connect server\n%!" (Clock.time ());
(*             Datagram.UDPv4.recv mgr (None, port) echo_udp *)
          Net.Channel.listen mgr (`TCPv4 ((None, port), echo ))
        | 2 -> 
          let dst_ip = Nettypes.ipv4_addr_of_tuple (10l,0l,1l,1l) in  
          Printf.printf "%f: trying to connect client \n%!" (Clock.time ());
          lwt _ = Manager.configure interface (`IPv4 (ip host_id)) in
          lwt _ = Time.sleep 1.0 in
          Printf.printf "%f: trying to connect client \n%!" (Clock.time ());
(*             echo_client_udp mgr (dst_ip,port) *)
          Net.Channel.connect mgr (`TCPv4 (None, (dst_ip, port), echo_client )) <&>
            (print_time ())
        | _ -> return (printf "Invalid node_id %d\n%!" host_id)
        )
    with e ->
      Printf.eprintf "Error: %s" (Printexc.to_string e); 
      return ()
  in  
    config_host host_id

(* Design the topology *)
let run () =
  let _ = OS.Time.set_duration 10 in 
  (* Define participating nodes *)
  let _ = Topology.add_node "node1" (host_inner 1) in
  let _ = Topology.add_node "node2" (host_inner 2) in

  (* Define topology *)
  let _ = Topology.add_link "node1" "node2" in
    ()
