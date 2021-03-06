(*
 * Copyright (c) 2011 Anil Madhavapeddy <anil@recoil.org>
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

(* The manager process binds application ports to interfaces, and
   will eventually deal with load balancing and route determination
   (e.g. if a remote target is on the same host, swap to shared memory *)

open Lwt
open Nettypes
open Printf

exception Error of string

type id = OS.Netif.id

type interface = {
  id: id;
  netif: Ethif.t;
  ipv4: Ipv4.t;
  icmp: Icmp.t;
  udp: Udp.t;
  tcp: Tcp.Pcb.t;
}

type interface_t = interface * unit Lwt.t

type t = {
  listener: t -> interface -> id -> unit Lwt.t;
  listeners: (id, interface_t) Hashtbl.t;
}

type config = [ `DHCP | `IPv4 of ipv4_addr * ipv4_addr * ipv4_addr list ]

(* Configure an interface based on the Config module *)
let configure i =
  function
  |`DHCP ->
    printf "Manager: VIF %s to DHCP\n%!" i.id;
    lwt t, th = Dhcp.Client.create i.ipv4 i.udp in
    printf "Manager: DHCP done\n%!";
    return ()
  |`IPv4 (addr, netmask, gateways) ->
    printf "Manager: VIF %s to %s nm %s gw [%s]\n%!"
      i.id (ipv4_addr_to_string addr) (ipv4_addr_to_string netmask)
      (String.concat ", " (List.map ipv4_addr_to_string gateways));
    Ipv4.set_ip i.ipv4 addr >>
    Ipv4.set_netmask i.ipv4 netmask >>
    Ipv4.set_gateways i.ipv4 gateways >>
    return ()

(* Plug in a new network interface with given id *)
let plug t id vif =
  printf "Manager: plug %s\n%!" id; 
  let wrap (s,t) = try_lwt t >>= return with exn ->
    (printf "Manager: exn=%s %s\n%!" s (Printexc.to_string exn); fail exn) in
  let (netif, netif_t) = Ethif.create vif in
  let (ipv4, ipv4_t) = Ipv4.create netif in
  let (icmp, icmp_t) = Icmp.create ipv4 in
  let (tcp, tcp_t) = Tcp.Pcb.create ipv4 in
  let (udp, udp_t) = Udp.create ipv4 in
  let i = { id; ipv4; icmp; netif; tcp; udp } in
  (* The interface thread can be cancelled by exceptions from the
     rest of the threads, as a debug measure.
     TODO: think about restart strategies here *)
  let th,_ = Lwt.task () in
  (* Register the interface_t with the manager interface *)
  Hashtbl.add t.listeners id (i,th);
  printf "Manager: plug done, to listener\n%!";
  t.listener t i id

let inject_packet t id buf = 
 try_lwt
    let (th, _) = Hashtbl.find t.listeners id in 
      Ethif.write th.netif buf 
  with exn ->
    return (eprintf "Net.Manager.inject_packet : %s\n%!" 
              (Printexc.to_string exn))

(* Unplug a network interface and cancel all its threads. *)
let unplug t id =
  try
    let i, th = Hashtbl.find t.listeners id in
      Lwt.cancel th;
      Hashtbl.remove t.listeners id
  with Not_found -> ()

(* Enumerate interfaces and manage the protocol threads.
 The listener becomes a new thread that is spawned when a 
 new interface shows up. *)
let create listener =
  printf "Manager: create\n%!";
  let listeners = Hashtbl.create 1 in
  let t = { listener; listeners } in
  let _ = OS.Netif.create (plug t) in
  let th,_ = Lwt.task () in
  Lwt.on_cancel th (fun _ ->
    printf "Manager: cancel\n%!";
    Hashtbl.iter (fun id _ -> unplug t id) listeners);
  printf "Manager: init done\n%!";
  th

(* Find the interfaces associated with the address *)
let i_of_ip t addr =
  match addr with
    |None ->
        Hashtbl.fold (fun _ (i,_) a ->
                        i :: a) t.listeners []
    |Some addr -> begin
       Hashtbl.fold (fun _ (i,_) a ->
                       if Ipv4.get_ip i.ipv4 = addr then i :: a else a
       ) t.listeners [];
     end

let match_ip_match ip netmask dst_ip =
  let src_match = Int32.logand ip netmask in 
  let dst_match = Int32.logand dst_ip netmask in
    (src_match = dst_match)

(* Get an appropriate interface for a dest ip *)
let i_of_dst_ip t addr =
  let ret = ref None in 
  let netmask = ref 0l in   
  let _ = Hashtbl.iter 
      (fun _ (i,_) ->
         let l_ip =  Ipv4.get_ip i.ipv4 in
         let l_mask = Ipv4.get_netmask i.ipv4 in 
           (* Need to consider also default gateways as 
           * well as same subnet forwarding *)
          if (( (Int32.logor (!netmask) l_mask) <> !netmask) && 
               (match_ip_match l_ip l_mask addr)) then (
                 ret := Some(i);
                 netmask :=  Ipv4.get_netmask i.ipv4
               )
      ) t.listeners in
    match !ret with
      | None -> failwith("No_Path_dst")
      | Some(ret) -> ret

(* Match an address and port to a TCP thread *)
let tcpv4_of_addr t addr =
  List.map (fun x -> x.tcp) (i_of_ip t addr)

let tcpv4_of_dst_addr t addr =
  let x = i_of_dst_ip t addr in
    x.tcp

(* TODO: do actual route selection *)
let udpv4_of_addr (t:t) addr =
  List.map (fun x -> x.udp) (i_of_ip t addr)
let ipv4_of_interface (t:interface) = 
  t.ipv4

let get_intf_name t id = 
 try
    let (intf, _) = Hashtbl.find t.listeners id in 
      intf.id 
  with exn ->
    eprintf "Net.Manager.get_intf_name : %s\n%!" 
              (Printexc.to_string exn);
    ""

let get_intf_mac t id = 
 try
    let (th, _) = Hashtbl.find t.listeners id in 
      th.netif.Ethif.mac 
  with exn ->
    eprintf "Net.Manager.get_intf_mac : %s\n%!" 
              (Printexc.to_string exn);
    ""

let set_promiscuous t id f =
 try
   let (th, _) = Hashtbl.find t.listeners id in 
     Ethif.set_promiscuous th.netif (f id)
  with exn ->
    eprintf "Net.Manager.get_intf_mac : %s\n%!" 
              (Printexc.to_string exn)
