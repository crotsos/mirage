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
open Net
open Net.Nettypes

module OP = Ofpacket
module OC = Controller 
module OE = Controller.Event 
open OP

let sp = Printf.sprintf
let pr = Printf.printf
let pp = Printf.printf
let ep = Printf.eprintf
let cp = OS.Console.log

(* fake switch state to be exposed to controllers *)
type port = {
  port_id: int;
  port_name: string;
  phy: OP.Port.phy;
  origin_dpid: int64;
  origin_port_id: int;
}

type flv_dpid = 
  | Ofc of int64
  | Ofs of int64

type t = {
  mutable ports: port list; 
  mutable errornum : int32; 
  mutable portnum : int;
  mutable xid_count : int32;
  
  (* Wildcard matches for each dpid *)
  mutable dpid_to_match : (int64 * OP.Match.t) list;
  
  (* Store where I should send a reply back *)
  xid_to_dpid : (int32, int64) Hashtbl.t;
  
  (* dpid mapping to writable socket *)
  dpid_to_ch : (flv_dpid, Net.Channel.t) Hashtbl.t; 
  
  (*maping a flowvisor port_id to a real port_id of a switch *)
  port_id_to_switch : (int, (int64 * int )) Hashtbl.t;
  features : OP.Switch.features;
  
  (* Store packet in packet. This will allow to propagate a packet buffer_id
  * between all the participating switch, even the ones that don't had the
  * relevant buffer_id *)
  mutable packet_buffer: (OP.Packet_in.t * int64) list;
  mutable controllers: (int64 * (ipv4_addr * int)) list;

  buffer_id_map : (int32, (int32 * int64)) Hashtbl.t;
  mutable buffer_id_count: int32;

  flv_topo: Flowvisor_topology.t;
}

let supported_actions () = 
  OP.Switch.({ output=true; set_vlan_id=true; set_vlan_pcp=true; strip_vlan=true;
               set_dl_src=true; set_dl_dst=true; set_nw_src=true; set_nw_dst=true;
               set_nw_tos=true; set_tp_src=true; set_tp_dst=true; enqueue=false;
               vendor=true; })
let supported_capabilities () = 
  OP.Switch.({flow_stats=true;table_stats=true;port_stats=true;stp=true;
              ip_reasm=false;queue_stats=false;arp_match_ip=true;})
let switch_features () = 
  OP.Switch.({datapath_id=1L; n_buffers=0l; n_tables=(char_of_int 1); 
              capabilities=(supported_capabilities ()); 
              actions=(supported_actions ()); ports=[];})
let init_flowvisor flv_topo = 
  {ports=[]; errornum=0l; portnum=0; xid_count=0l; dpid_to_match=[];
   port_id_to_switch=(Hashtbl.create 64); features=(switch_features ());
   packet_buffer=[]; controllers=[]; buffer_id_map=(Hashtbl.create 64);
   buffer_id_count=0l; xid_to_dpid=(Hashtbl.create 64); 
   dpid_to_ch=(Hashtbl.create 64); flv_topo; }

let supported_actions () = 
  OP.Switch.({ output=true; set_vlan_id=true; set_vlan_pcp=true; strip_vlan=true;
               set_dl_src=true; set_dl_dst=true; set_nw_src=true; set_nw_dst=true;
               set_nw_tos=true; set_tp_src=true; set_tp_dst=true; enqueue=false;
               vendor=true; })
let supported_capabilities () = 
  OP.Switch.({flow_stats=true;table_stats=true;port_stats=true;stp=true;
              ip_reasm=false;queue_stats=false;arp_match_ip=true;})
let switch_features () = 
  OP.Switch.({datapath_id=1L; n_buffers=0l; n_tables=(char_of_int 1); 
              capabilities=(supported_capabilities ()); 
              actions=(supported_actions ()); ports=[];})

(*************************************************
 * Switch OpenFlow control channel 
 *************************************************)

let match_dpid_buffer_id st dpid buffer_id = 
  try
    let (_, dst_dpid) = Hashtbl.find st.buffer_id_map buffer_id in 
      (dpid = dst_dpid)
  with Not_found -> false

let send_to_all_switches st bits =
  let channels = 
    Hashtbl.fold 
      (fun dpid ch c -> 
         match dpid with 
           | Ofs(dpid) -> c @[ch]
           | Ofc(dpid) -> c)
      st.dpid_to_ch []
  in
  Lwt_list.iter_p (
    fun ch -> 
      let _ = Channel.write_buffer ch bits in 
        Channel.flush ch) channels 

let send_to_switch st dpid bits = 
  let ch = Hashtbl.find st.dpid_to_ch (Ofs(dpid)) in 
  let _ = Channel.write_buffer ch bits in 
    Channel.flush ch 

let get_new_xid st dpid = 
  let xid = st.xid_count in 
  let _ = st.xid_count <- Int32.add st.xid_count 1l in 
  let _ = Hashtbl.add st.xid_to_dpid xid dpid in
    xid

let map_ports in_port out_port = 
  (0L, in_port, out_port)

let process_openflow st dpid t bits =  function
  | OP.Hello (h) -> 
    (* Reply to HELLO with a HELLO and a feature request *)
    cp "HELLO";
    return ()
  | OP.Echo_req (h, bs) -> (* Reply to ECHO requests *)
    cp "ECHO_REQ";
    let h = OP.Header.(create ECHO_RESP sizeof_ofp_header h.xid) in
    let bits = OP.marshal_and_sub (OP.Header.marshal_header h)
      (OS.Io_page.get ()) in 
    let _ = Channel.write_buffer t bits in 
      Channel.flush t
  | OP.Features_req (h)  -> 
    cp "FEAT_REQ";
    let bits = OP.marshal_and_sub
      (OP.Switch.marshal_reply_features h.OP.Header.xid st.features )
      (OS.Io_page.get ()) in 
    let _ = Channel.write_buffer t bits in
      Channel.flush t
  | OP.Stats_req(h, req) -> begin
      (* TODO Need to translate the xid here *)
    let xid = h.OP.Header.xid in 
    cp "STATS_REQ\n%!";
    match req with
    | OP.Stats.Desc_req(req) ->
      let desc = OP.Stats.({ imfr_desc="Mirage"; hw_desc="Mirage";
                  sw_desc="Mirage_flowvisor"; serial_num="0.1";dp_desc="Mirage";}) 
      in  
      let resp_h = OP.Stats.({st_ty=DESC; more_to_follow=false;}) in 
      let bits =  OP.marshal_and_sub 
                  (OP.Stats.marshal_stats_resp xid (OP.Stats.Desc_resp(resp_h,
                  desc))) 
                  (OS.Io_page.get ()) in   
      let _ = Channel.write_buffer t bits in
        Channel.flush t 
    | OP.Stats.Flow_req(req_h, of_match, table_id, out_port) -> begin
      (*TODO Need to consider the  table_id and the out_port and 
       * split reply over multiple openflow packets if they don't
       * fit a single packet. *)
        let xid = get_new_xid st dpid in 
          match (of_match.OP.Match.wildcards.OP.Wildcards.in_port,  
                 (of_match.OP.Match.in_port)) with
            | (false, OP.Port.Port(p)) ->
                let (dpid, out_port) = Hashtbl.find st.port_id_to_switch p in
                let out_port = OP.Port.Port(out_port) in 
                let of_match = OP.Match.translate_port of_match out_port in 
                (* TODO out_port needs processing. if dpid are between different 
                * switches need to define the outport as the port of the
                * interconnection link *)
                let table_id = OP.Stats.int_of_table_id table_id in 
                let data = 
                  OP.marshal_and_sub 
                    (OP.Stats.create_flow_stat_req of_match ~table_id ~out_port ~xid)
                    (OS.Io_page.get ()) in
                  send_to_switch st dpid data
            | (_, _) -> 
                let data = 
                  OP.marshal_and_sub 
                    (OP.Stats.create_flow_stat_req of_match 
                       ~table_id:(OP.Stats.int_of_table_id table_id) ~out_port ~xid)
                    (OS.Io_page.get ()) in
                  send_to_all_switches st data
      end
    | OP.Stats.Aggregate_req (req_h, of_match, table_id, out_port) -> begin
        let xid = get_new_xid st dpid in 
          match (of_match.OP.Match.wildcards.OP.Wildcards.in_port,  
                 (of_match.OP.Match.in_port)) with
            | (false, OP.Port.Port(p)) ->
                let (dpid, port) = Hashtbl.find st.port_id_to_switch p in 
                let port = OP.Port.Port(port) in 
                let of_match = OP.Match.translate_port of_match port in 
                (* TODO out_port needs processing. if dpid are between different 
                * switches need to define the outport as the port of the
                * interconnection link *)
                let data = 
                  OP.marshal_and_sub 
                    (OP.Stats.create_aggr_flow_stat_req of_match 
                       ~table_id:(OP.Stats.int_of_table_id table_id) ~out_port ~xid)
                    (OS.Io_page.get ()) in
                  send_to_switch st dpid data 
            | (_, _) ->
                let data = 
                  OP.marshal_and_sub 
                    (OP.Stats.create_aggr_flow_stat_req of_match 
                       ~table_id:(OP.Stats.int_of_table_id table_id) ~out_port ~xid)
                    (OS.Io_page.get ()) in
                  send_to_all_switches st data
      end
    | OP.Stats.Table_req(req) ->
        let xid = get_new_xid st dpid in 
        let data =  
          OP.marshal_and_sub 
            (OP.Stats.create_table_stat_req ~xid) (OS.Io_page.get ()) in
          send_to_all_switches st data 
    | OP.Stats.Port_req(req_h, port) -> begin
      match port with
      | OP.Port.No_port -> 
        let xid = get_new_xid st dpid in 
        let data = 
          OP.marshal_and_sub 
            (OP.Stats.create_port_stat_req ~port ~xid) (OS.Io_page.get ()) in
          send_to_all_switches st data 
      | OP.Port.Port(port) -> 
        try_lwt 
          let xid = get_new_xid st dpid in 
          let (dpid, port) = Hashtbl.find st.port_id_to_switch port in 
          let data = 
            OP.marshal_and_sub 
              (OP.Stats.create_port_stat_req ~port:(OP.Port.Port(port)) ~xid) 
              (OS.Io_page.get ()) in
            send_to_switch st dpid data
        with Not_found ->
          (* TODO reply with right error code *)
          pr "Invalid port_id in stats\n%!";
          return ()
        end
      | _ -> 
        let bits = OP.marshal_and_sub (OP.marshal_error  
                    OP.REQUEST_BAD_SUBTYPE bits xid) (OS.Io_page.get ()) in
        let _ = Channel.write_buffer t bits in 
          Channel.flush t
  end
  | OP.Get_config_req(h) ->
      (* TODO make a custom reply tothe query *) 
    let resp = OP.Switch.init_switch_config in
    let bits = OP.marshal_and_sub (OP.Switch.marshal_switch_config 
                h.OP.Header.xid resp) (OS.Io_page.get ()) in
    let _ = Channel.write_buffer t bits in 
      Channel.flush t
  | OP.Barrier_req(h) ->
      (* TODO just reply for now. need to check this with all switches *)
(*       let xid = get_new_xid dpid in  *)
      let _ = cp (sp "BARRIER_REQ: %s\n%!" (OP.Header.header_to_string h)) in
      let resp_h = (OP.Header.create OP.Header.BARRIER_RESP
                      (OP.Header.sizeof_ofp_header) h.OP.Header.xid) in
      let bits = OP.marshal_and_sub (OP.Header.marshal_header resp_h)        
                   (OS.Io_page.get ()) in
      let _ = Channel.write_buffer t bits in 
        Channel.flush t
  | OP.Packet_out(h, pkt) -> begin
      let xid = get_new_xid st dpid in 
      let _ = cp (sp "PACKET_OUT: %s\n%!" (OP.Packet_out.packet_out_to_string pkt)) in
    (* Check if controller has the right to send traffic on the specific subnet *)
      (* if no buffer found, find the output port and send the packet 
      * directly to the node *)
      let rec action_parse new_actions = function
        | (OP.Flow.Output(p, len))::tail -> 
            let (dpid, in_port, port) = map_ports pkt.OP.Packet_out.in_port p in
            let actions = new_actions @ [OP.Flow.Output(port, len)] in
            let data = 
              match (pkt.OP.Packet_out.buffer_id) with
                | -1l -> 
                    let m = (OP.Packet_out.create ~xid ~buffer_id:(-1l) ~actions 
                                ~in_port ~data:pkt.OP.Packet_out.data ()) in
                    marshal_and_sub (OP.Packet_out.marshal_packet_out m) 
                      (OS.Io_page.get ())
                | buffer_id when (match_dpid_buffer_id st dpid buffer_id) -> 
                    let m = (OP.Packet_out.create ~xid ~buffer_id ~actions 
                        ~in_port ~data:pkt.OP.Packet_out.data ()) in
                    marshal_and_sub (OP.Packet_out.marshal_packet_out m)
                      (OS.Io_page.get ())
                | buffer_id  -> 
                    (*find buffer_id and get data to append in the packet *)
                    let m = (OP.Packet_out.create ~xid ~buffer_id ~actions 
                               ~in_port ~data:pkt.OP.Packet_out.data ()) in
                     marshal_and_sub (OP.Packet_out.marshal_packet_out m) 
                      (OS.Io_page.get ())
              in 
              lwt _ = send_to_switch st dpid data in
                action_parse new_actions tail 
        | a :: tail -> 
            let new_actions = new_actions @ [a] in 
              action_parse new_actions tail 
        | [] -> return ()
      in 
        action_parse [] pkt.OP.Packet_out.actions 
    end
  | OP.Flow_mod(h,fm)  ->
      (* check in and out port and find if both port are on same switch.*)
      (* if the dpid are different, find a dpid path and install each flow *)
      (* if dpid is the same, simply map ports and send packet *)
    cp (sp "FLOW_MOD: %s\n%!" (OP.Flow_mod.flow_mod_to_string fm));
(*    let of_match = fm.OP.Flow_mod.of_match in 
    let of_actions = fm.OP.Flow_mod.actions in
    lwt _ = 
      match (fm.OP.Flow_mod.command) with
      | OP.Flow_mod.ADD 
      | OP.Flow_mod.MODIFY 
      | OP.Flow_mod.MODIFY_STRICT -> 
        return (Table.add_flow st.Switch.table fm)
      | OP.Flow_mod.DELETE 
      | OP.Flow_mod.DELETE_STRICT ->
        Table.del_flow st.Switch.table t of_match fm.OP.Flow_mod.out_port
    in 
      if (fm.OP.Flow_mod.buffer_id = -1l) then
        return () 
      else begin
        let pkt_in = ref None in 
        let _ = 
          st.Switch.packet_buffer <- 
          List.filter (
            fun a -> 
              if (a.OP.Packet_in.buffer_id = fm.OP.Flow_mod.buffer_id) then 
                (pkt_in := Some(a); false )
              else true 
          ) st.Switch.packet_buffer
        in 
          match (!pkt_in) with 
            | None -> 
                let bs = 
                  OP.marshal_and_sub 
                    (OP.marshal_error OP.REQUEST_BUFFER_UNKNOWN bits h.OP.Header.xid)
                    (OS.Io_page.get ()) in 
                let _ = Channel.write_buffer t bs in 
                  Channel.flush t 
            | Some(pkt_in) ->
                (* TODO check if the match is accurate? *)
                Switch.apply_of_actions st pkt_in.OP.Packet_in.in_port
                  pkt_in.OP.Packet_in.data of_actions 
      end *)
    return ()
  | OP.Queue_get_config_resp (h, _, _)
  | OP.Queue_get_config_req (h, _)
  | OP.Barrier_resp h
  | OP.Stats_resp (h, _)
  | OP.Port_mod (h, _)
  | OP.Port_status (h, _)
  | OP.Flow_removed (h, _)
  | OP.Packet_in (h, _)
  | OP.Set_config (h, _)
  | OP.Get_config_resp (h, _)
  | OP.Features_resp (h, _)
  | OP.Vendor (h, _, _)
  | OP.Echo_resp (h, _)
  | OP.Error (h, _) ->
    let bits = OP.marshal_and_sub (OP.marshal_error         
                OP.REQUEST_BAD_TYPE bits h.OP.Header.xid) 
                (OS.Io_page.get ()) in
    let _ = Channel.write_buffer t bits in 
      Channel.flush t

(* let control_channel st (remote_addr, remote_port) t =
  let rs = Nettypes.ipv4_addr_to_string remote_addr in
  Log.info "OpenFlow Switch: controller " "+ %s:%d" rs remote_port; 

  (* Trigger the dance between the 2 nodes *)
  let h = OP.Header.(create HELLO sizeof_ofp_header 1l) in  
  let bits = OP.marshal_and_sub (OP.Header.marshal_header h)
               (OS.Io_page.get ()) in 
  let _ = Channel.write_buffer t bits in 
  lwt _ = Channel.flush t in 
  let cached_socket = Ofsocket.create_socket t in 

  let rec echo () =
    try_lwt
      lwt hbuf = Ofsocket.read_data cached_socket OP.Header.sizeof_ofp_header in
        let ofh  = OP.Header.parse_header  hbuf in
        let dlen = ofh.OP.Header.len - OP.Header.sizeof_ofp_header in 
        lwt dbuf = Ofsocket.read_data cached_socket dlen in
        let ofp  = OP.parse ofh dbuf in
          process_openflow st t dbuf ofp (* Bitstring.concat [hbuf; dbuf] *) 
          >> echo ()
    with
    | Nettypes.Closed -> 
            (* TODO Need to remove the t from st.Switch.controllers *)
        pr "Controller channel closed....\n%!"; 
        return ()
    | OP.Unparsed (m, bs) -> (pr "# unparsed! m=%s\n %!" m); echo ()
    | exn -> 
        pr "[OpenFlow-Switch-Control] ERROR:%s\n" (Printexc.to_string exn);
        (echo () ) 

  in 
    echo () <&> (Table.monitor_flow_timeout st.Switch.table t) *)

let switch_channel st dpid (remote_addr, remote_port) t =
  let rs = Nettypes.ipv4_addr_to_string remote_addr in
  Log.info "OpenFlow Switch: controller " "+ %s:%d" rs remote_port; 
(*   st.Switch.controllers <- (st.Switch.controllers @ [t]); *)

  (* Trigger the dance between the 2 nodes *)
  let h = OP.Header.(create HELLO sizeof_ofp_header 1l) in  
  let bits = OP.marshal_and_sub (OP.Header.marshal_header h)
               (OS.Io_page.get ()) in 
  let _ = Channel.write_buffer t bits in 
  lwt _ = Channel.flush t in 
  let cached_socket = Ofsocket.create_socket t in 

  let rec echo () =
    try_lwt
      lwt hbuf = Ofsocket.read_data cached_socket OP.Header.sizeof_ofp_header in
        let ofh  = OP.Header.parse_header  hbuf in
        let dlen = ofh.OP.Header.len - OP.Header.sizeof_ofp_header in 
        lwt dbuf = Ofsocket.read_data cached_socket dlen in
        let ofp  = OP.parse ofh dbuf in
          process_openflow st dpid t dbuf ofp (* Bitstring.concat [hbuf; dbuf] *) 
          >> echo ()
    with
    | Nettypes.Closed -> 
            (* TODO Need to remove the t from st.Switch.controllers *)
        pr "Controller channel closed....\n%!"; 
        return ()
    | OP.Unparsed (m, bs) -> (pr "# unparsed! m=%s\n %!" m); echo ()
    | exn -> 
        pr "[OpenFlow-Switch-Control] ERROR:%s\n" (Printexc.to_string exn);
        (echo () ) 
  in 
    echo () 

(*
 * openflow controller threads 
 * *)

let process_switch_channel flv st dpid e = 
  match e with 
    | OE.Datapath_join(dpid, ports) ->
        let _ = pr "New switch connected...\n%!" in 
        let _ = Flowvisor_topology.add_channel flv.flv_topo dpid st in
        lwt _ = Lwt_list.iter_p (
          fun p -> 
            Flowvisor_topology.add_port flv.flv_topo dpid p.OP.Port.port_no
              p.OP.Port.hw_addr  
        ) ports in
          return ()
    | OE.Packet_in(p, buffer_id, pkt, dpid) ->
        let m = OP.Match.raw_packet_to_match p pkt in 
        lwt _ = 
          match (p, m.OP.Match.dl_type) with 
            | (OP.Port.Port(p), 0x88cc) -> 
                let _ = pr "LLDP packet received by controller \n%!" in 
                let _ = Flowvisor_topology.process_lldp_packet 
                          flv.flv_topo dpid p pkt in 
                  return ()
            | _ -> return ()
        in
          pr "packet in received by switch %Ld\n%!" dpid; 
          return ()
    | OE.Datapath_leave(dpid) ->
        let _ = Flowvisor_topology.remove_dpid flv.flv_topo dpid in
        (* Need to remove ports and port mapping and disard any state pending 
         * for replies. *)  
         return ()
    | OE.Flow_removed(of_m, r, dur_s, dur_ns, pkts, bytes, dpid) -> 
          (* Send packet to all controllers *)
        return ()
    | OE.Flow_stats_reply(xid, more, flows, dpid) ->
          (* Group reply separation *)
        return ()
    | OE.Aggr_flow_stats_reply(xid, pkts, bytes, flows, dpid) -> 
        return ()
    | OE.Port_stats_reply(xid, ports, dpid) -> 
        return ()
    | OE.Table_stats_reply(xid, tables, dpid) -> 
        return ()
    | OE.Port_status(reason, port, dpid) -> 
          (* send a port withdrawal to all controllers *)
        return ()
    | _ -> return (pp "Unsupported event on controller channel...\n%!")

let init flv st = 
  (* register all the required handlers *)
  let _ = OC.register_cb st OE.DATAPATH_JOIN (process_switch_channel flv) in 
  let _ = OC.register_cb st OE.DATAPATH_LEAVE (process_switch_channel flv) in 
  let _ = OC.register_cb st OE.PACKET_IN (process_switch_channel flv) in 
  let _ = OC.register_cb st OE.FLOW_REMOVED (process_switch_channel flv) in 
  let _ = OC.register_cb st OE.FLOW_STATS_REPLY (process_switch_channel flv) in 
  let _ = OC.register_cb st OE.AGGR_FLOW_STATS_REPLY 
            (process_switch_channel flv) in 
  let _ = OC.register_cb st OE.PORT_STATUS_CHANGE (process_switch_channel flv) in 
  let _ = OC.register_cb st OE.TABLE_STATS_REPLY (process_switch_channel flv) in 
  let _ = OC.register_cb st OE.PORT_STATUS_CHANGE (process_switch_channel flv) in 
    ()

let controller_channel mgr st = 
  Lwt_list.iter_p (
    fun (dpid, (ip, port)) -> 
      while_lwt true do 
        Net.Channel.connect mgr 
          ( `TCPv4 (None, (ip, port), (switch_channel st dpid (ip, port)) ) )
      done 
  ) st.controllers 

(*
 * exposed flowvisor API
 * *)

let create_flowvisor () = 
  let flv_topo = Flowvisor_topology.init_topology () in  
    init_flowvisor flv_topo 

let listen st mgr loc flv_init =
  lwt _ = flv_init st in 
     Controller.listen mgr loc (init st) <&> 
(*       controller_channel mgr st <&> *)
      Flowvisor_topology.discover st.flv_topo
let remove_slice _ _ = return ()
let add_slice _ _ _ = return ()
