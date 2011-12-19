(* Simple openflow controller that listens on port 6633 and replies
   with echo request on every packet_in event *)

open Lwt
open Printf
open Gc

let resolve t = Lwt.on_success t (fun _ -> ())

module OP = Openflow.Ofpacket
module OC = Openflow.Controller
module OE = OC.Event

let pp = Printf.printf
let sp = Printf.sprintf

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

let switch_data = { mac_cache = Hashtbl.create 0;
                    dpid = []; 
                    of_ctrl = [];
                  } 


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
  let m = OP.Match.parse_from_raw_packet in_port data in 

(*
  let pkt = OP.Packet_out.create
  ~buffer_id:buffer_id ~actions:[ OP.(Flow.Output(Port.All , 2000))] 
  ~data:data ~in_port:in_port () 
  in
  let bs = OP.Packet_out.packet_out_to_bitstring pkt in 
  OC.send_of_data controller dpid bs
*)
    (* save src mac address *)
(*   let ix = {addr= m.OP.Match.dl_src; switch=dpid;} in *)

(*
  let ix = m.OP.Match.dl_src in
  add_entry_in_hashtbl switch_data.mac_cache ix in_port;
 
*)
  (* check if I know the output port in order to define what type of message
   * we need to send *)
(*   let ix = {addr= (OP.Match.get_dl_dst m); switch=dpid;} in *)


 let ix = m.OP.Match.dl_dst in
  if ( (OP.eaddr_is_broadcast ix)
       || (not (Hashtbl.mem switch_data.mac_cache ix)) ) 
  then (
    let pkt = OP.Packet_out.create
      ~buffer_id:buffer_id ~actions:[ OP.(Flow.Output(Port.All , 2000))] 
      ~data:data ~in_port:in_port () 
    in
    let bs = OP.Packet_out.packet_out_to_bitstring pkt in 
        OC.send_of_data controller dpid bs
(*     Printf.fprintf switch_data.log "%d %f\n" (!req_count) (((OS.Clock.time ()) -. ts)*.1000000.0) *)
  ) else (
    let out_port = (Hashtbl.find switch_data.mac_cache ix) in
    let actions = [OP.Flow.Output(out_port, 2000)] in
    let pkt = OP.Flow_mod.create m 0_L OP.Flow_mod.ADD 
                ~buffer_id:(Int32.to_int buffer_id)
                actions () in 
    let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
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

let ip = Net.Nettypes.(
    (ipv4_addr_of_tuple (10l,0l,0l,1l),
    ipv4_addr_of_tuple (255l,255l,255l,0l),
    [ ipv4_addr_of_tuple (10l,0l,0l,2l) ]
    ))

let main () =
(*    Gc.set { (Gc.get()) with Gc.minor_heap_size = 128000000 };
    Gc.set { (Gc.get()) with Gc.major_heap_increment = 128000000 };
    Gc.set { (Gc.get()) with Gc.stack_limit = 128000000 };
    Gc.set { (Gc.get()) with Gc.allocation_policy = 0 };
    Gc.set { (Gc.get()) with Gc.space_overhead = 200 };*)

    Log.info "OF Controller" "starting controller";
    let t1 = Net.Manager.create (fun mgr interface id ->
(*         Net.Manager.configure interface (`IPv4 ip);  *)
         Net.Manager.configure interface (`DHCP);  

        let port = 6633 in
        let t1 = (OC.listen mgr (None, port) init) in 
        t1 >> return (Log.info "OF Controller" "done!"))
        in 
        (*     let t2 = terminate_controller () in *)
(*         let t3 = memory_debug () in   *)
        t1  (* <&> t3  *)
