(*
 * Copyright (c) 2011 Charalampos Rotsos <cr409@cl.cam.ac.uk> 
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
open Net.Nettypes
open Printf

module OP = Openflow.Ofpacket
module OC = Openflow.Controller
module OE = OC.Event

let port = 55555
let use_dhcp = false

let pp = Printf.printf

(****************************************************************
 * OpenFlow Switch configuration 
 *****************************************************************)

let print_time () =
  while_lwt true do
    Time.sleep 1.0 >>
    return (printf "%03.6f: process running..\n%!" (OS.Clock.time ()))
  done

let switch_inner switch_id () = 
  let sw = Openflow.Ofswitch.create_switch (Int64.of_int switch_id) in
  try_lwt 
    Manager.create 
    (fun mgr interface id ->
       match (id) with 
         | "0" ->
             let ip = 
               Nettypes.(
                 (ipv4_addr_of_tuple (10l,0l,(Int32.of_int switch_id),2l),
                  ipv4_addr_of_tuple (255l,255l,255l,0l), [])) in  
               lwt _ = Manager.configure interface (`IPv4 ip) in
               let dst_ip = ipv4_addr_of_tuple (10l,0l,(Int32.of_int switch_id),1l) in
               lwt _ = (Openflow.Ofswitch.connect sw mgr (dst_ip, 6633)) in 
                return ()
      | _ -> 
          return (Openflow.Ofswitch.add_port mgr sw id)
    )
  with e ->
    Printf.eprintf "Error: %s" (Printexc.to_string e); 
    return ()

(*
 * flow visor node
 * *)
let init_slice st = 
  return ()

let flowvisor_inner () = 
  let sw = Openflow.Flowvisor.create_flowvisor () in
  try_lwt
    let t, u = Lwt.task () in 
    let running = ref false in 
    Manager.create 
      (fun mgr interface id ->
         let ip = 
           Nettypes.(
             (ipv4_addr_of_tuple (10l,0l,(Int32.of_string id),1l),
              ipv4_addr_of_tuple (255l,255l,255l,0l), [])) in  
         lwt _ = Manager.configure interface (`IPv4 ip) in
           match id with
             | "1" -> 
                 lwt _ = (Openflow.Flowvisor.listen sw mgr 
                           (None, 6633) init_slice) <&> 
                       (print_time ()) in
                 let _ = Lwt.wakeup u () in 
                   return ()
            | _ -> t
      )
  with e ->
    Printf.eprintf "Error: %s" (Printexc.to_string e); 
    return ()

(* Design the topology *)
let run () =
  let _ = OS.Time.set_duration 10 in 
  (* Define participating nodes *)
  let _ = Topology.add_node "switch1" (switch_inner 0) in 
  let _ = Topology.add_node "switch2" (switch_inner 1) in 
  (*let _ = Topology.add_node "switch3" (switch_inner 2) in 
  let _ = Topology.add_node "switch4" (switch_inner 3) in *)
  let _ = Topology.add_node "flv" flowvisor_inner in 

  (* Define topology *)
(*   let _ = Topology.add_link "controller" "flv" in *)
  let _ = Topology.add_link ~rate:1000 ~pcap:true "switch1" "flv" in
  let _ = Topology.add_link ~rate:1000 ~pcap:true "switch2" "flv" in
(*  let _ = Topology.add_link ~rate:1000 ~pcap:true "switch3" "flv" in
  let _ = Topology.add_link ~rate:1000 ~pcap:true "switch4" "flv" in *)
  let _ = Topology.add_link ~rate:1000 ~pcap:true "switch1" "switch2" in
(*   let _ = Topology.add_link ~rate:1000 "switch2" "switch3" in *)
(*   let _ = Topology.add_link ~rate:1000 "switch3" "switch4" in *)
    ()
