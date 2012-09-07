open Lwt
open OS
open Net
open Printf

module OP = Openflow.Ofpacket
module OC = Openflow.Controller
module OE = OC.Event

let pp = Printf.printf
let sp = Printf.sprintf

let port = 6633

(* TODO this the mapping is incorrect. the datapath must be moved to the key
 * of the hashtbl *)
type mac_switch = {
  addr: OP.eaddr; 
  switch: OP.datapath_id;
}

type switch_state = {
(*   mutable mac_cache: (mac_switch, OP.Port.t) Hashtbl.t; *)
  mutable mac_cache: (OP.eaddr, OP.Port.t) Hashtbl.t; 
  mutable dpid: OP.datapath_id list;
  mutable of_ctrl: OC.state list; 
}

let switch_data = 
  { mac_cache = Hashtbl.create 0;
  dpid = []; of_ctrl = []; } 

let datapath_join_cb controller dpid evt =
  let dp = 
    match evt with
      | OE.Datapath_join c -> c
      | _ -> invalid_arg "bogus datapath_join event match!" 
  in
  switch_data.dpid <- switch_data.dpid @ [dp];
  return (pp "+ datapath:0x%012Lx\n" dp)

let req_count = (ref 0)

let add_entry_in_hashtbl mac_cache ix in_port = 
  if not (Hashtbl.mem mac_cache ix ) then
      Hashtbl.add mac_cache ix in_port
  else  
      Hashtbl.replace mac_cache ix in_port 

let packet_in_cb controller dpid evt =
  incr req_count;
  let ts = (OS.Clock.time () ) in 
  let (in_port, buffer_id, data, dp) = 
    match evt with
      | OE.Packet_in (inp, buf, dat, dp) -> (inp, buf, dat, dp)
      | _ -> invalid_arg "bogus datapath_join event match!"
  in
  (* Parse Ethernet header *)
  let m = OP.Match.raw_packet_to_match in_port data in  
(*     return () *)
  let ix = m.OP.Match.dl_dst in
  if ( (OP.eaddr_is_broadcast ix)
       || (not (Hashtbl.mem switch_data.mac_cache ix)) ) 
  then (
    let pkt = OP.Packet_out.create
      ~buffer_id:buffer_id ~actions:[ OP.(Flow.Output(Port.All , 2000))] 
      ~data:data ~in_port:in_port () 
    in
    let bits = OP.marshal_and_sub (OP.Packet_out.marshal_packet_out pkt )
          (OS.Io_page.get ()) in 
        OC.send_of_data controller dpid bits
  ) else (
    let out_port = (Hashtbl.find switch_data.mac_cache ix) in
    let actions = [OP.Flow.Output(out_port, 2000)] in
    let pkt = OP.Flow_mod.create m 0_L OP.Flow_mod.ADD 
                ~buffer_id:(Int32.to_int buffer_id)
                actions () in 
    let bs = (OS.Io_page.get ()) in
    let _ = OP.Flow_mod.marshal_flow_mod pkt bs in
      OC.send_of_data controller dpid bs
 ) 


let memory_debug () = 
   while_lwt true do
     (OS.Time.sleep 1.0)  >> 
     return (OC.mem_dbg "memory usage")
   done 

(*let terminate_controller controller =
  while_lwt true do
    (OS.Time.sleep 60.0)  >>
    exit(1) *)
(*    return (List.iter (fun ctrl -> Printf.printf "terminating\n%!";
 *    (OC.terminate ctrl))  *)
(*  switch_data.of_ctrl)  *)
(*   done *)

let init controller = 
  if (not (List.mem controller switch_data.of_ctrl)) then
    switch_data.of_ctrl <- (([controller] @ switch_data.of_ctrl));
  pp "test controller register datapath cb\n";
  OC.register_cb controller OE.DATAPATH_JOIN datapath_join_cb;
  pp "test controller register packet_in cb\n";
  OC.register_cb controller OE.PACKET_IN packet_in_cb

(* Plug in a new network interface with given id *)
let plug t id vif =
  printf "Manager: plug %s\n%!" id; 
  let wrap (s,t) = try_lwt t >>= return with exn ->
    (printf "Manager: exn=%s %s\n%!" s (Printexc.to_string exn); fail exn) in
  let (netif, netif_t) = Ethif.create ~promiscuous:(Some(capture_packet)) 
    vif in
  let th,_ = Lwt.task () in
  (* Register the interface_t with the manager interface *)
  Hashtbl.add t.listeners id (i,th);
  printf "Manager: plug done, to listener\n%!";
  t.listener t i id

(* Code to run on the end node *)
let run_inner () =
  try_lwt 
    Net.Manager.create ~plug:my_plug (fun mgr interface id ->
      Printf.printf "External intf for node 1\n%!";
      let ip = 
        Net.Nettypes.(
          (ipv4_addr_of_tuple (10l,0l,1l,2l),
          ipv4_addr_of_tuple (255l,255l,255l,0l),
          [ ipv4_addr_of_tuple (10l,0l,1l,1l) ]
          )) in     
      lwt _ = Manager.configure interface (`IPv4 ip) in
      let _ = printf "Intf configured...\n%!" in
      let t1 = (OC.listen mgr (None, port) init) in 
(*      lwt () = Net.Channel.listen mgr 
      (`TCPv4 ((None, port), echo)) in   *)
      return ()
      ) 
    with e ->
     Printf.eprintf "Error: %s" (Printexc.to_string e); 
     return ()

(* Design the topology *)
let run () =
  Time.set_duration 30;
  OS.Topology.add_node "node1" run_inner;
  OS.Topology.add_external_dev "nstap0" "node1" "10.0.1.1" "255.255.255.0"
