(*
 * Copyright (c) 2011 Richard Mortier <mort@cantab.net>
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
open Nettypes

module OP = Ofpacket

let sp = Printf.sprintf
let pr = Printf.printf
let ep = Printf.eprintf
let cp = OS.Console.log

(* XXX should really stndardise these *)
type uint16 = OP.uint16
type uint32 = OP.uint32
type uint64 = OP.uint64
type byte   = OP.byte
type eaddr  = OP.eaddr

type port = uint16
type cookie = uint64

type device = string (* XXX placeholder! *)

let resolve t = Lwt.on_success t (fun _ -> ())

module Entry = struct
  type table_counter = {
    n_active: uint32;
    n_lookups: uint64;
    n_matches: uint64;
  }

  type flow_counter = {
   mutable n_packets: uint64;
   mutable n_bytes: uint64;

   mutable priority: uint16;
   mutable cookie: int64;
   mutable insert_sec: uint32;
   mutable insert_nsec: uint32;
   mutable last_sec: uint32;
   mutable last_nsec: uint32;
   mutable idle_timeout: int;
   mutable hard_timeout:int;
  }

  type queue_counter = {
    tx_queue_packets: uint64;
    tx_queue_bytes: uint64;
    tx_queue_overrun_errors: uint64;
  }

  let init_flow_counters () =
    {n_packets=(Int64.of_int 0);
    n_bytes= (Int64.of_int 0);

    priority= 0; cookie= (Int64.of_int 0); 
    insert_sec=(Int32.of_float (OS.Clock.time()));
    insert_nsec=0l;
    last_sec=(Int32.of_float (OS.Clock.time())); 
    last_nsec=0l;
        idle_timeout=0; hard_timeout=0;}


  type t = { 
    counters: flow_counter;
    actions: OP.Flow.action list;
    mutable cache_entries: OP.Match.t list;
  }
  let update_flow pkt_len flow = 
    flow.counters.n_packets <- Int64.add flow.counters.n_packets 1L;
    flow.counters.n_bytes <- Int64.add flow.counters.n_bytes pkt_len;
    let ts = OS.Clock.time () in 
      flow.counters.last_sec <- (Int32.of_float ts)


  let flow_counters_to_flow_stats of_match table_id flow =
    let priority = flow.counters.priority in
    let idle_timeout=flow.counters.idle_timeout in
    let hard_timeout=flow.counters.hard_timeout in
    let cookie=flow.counters.cookie in
    let packet_count=flow.counters.n_packets in
    let byte_count=flow.counters.n_bytes in
    let action=flow.actions in
    OP.Flow.({table_id; of_match; 
    duration_sec=(Int32.sub flow.counters.last_sec flow.counters.insert_sec);
    duration_nsec=(Int32.sub flow.counters.last_nsec flow.counters.insert_nsec);
    priority; idle_timeout; hard_timeout; cookie;
    packet_count; byte_count; action; })

end

module Table = struct
  type t = {
    tid: cookie;
    (* This stores entries as they arrive *)
    mutable entries: (OP.Match.t, Entry.t) Hashtbl.t;
    (* This stores only exact match entries.*)
    (* TODO delete an entry from both tables *)
    mutable cache : (OP.Match.t, Entry.t ref) Hashtbl.t;
    stats : OP.Stats.table;
  }

  let update_table_found table = 
    table.stats.OP.Stats.lookup_count <- Int64.add
    table.stats.OP.Stats.lookup_count 1L;
    table.stats.OP.Stats.matched_count <- 
      Int64.add table.stats.OP.Stats.matched_count 1L

  let update_table_missed table =
    table.stats.OP.Stats.lookup_count <- Int64.add
    table.stats.OP.Stats.lookup_count 1L
end

module Switch = struct
  type port = {
    port_id: int;
    ethif: OS.Netif.t; 
    port_name: string;
    mgr: Net.Manager.t;
    counter: OP.Port.stats;
  }

  type stats = {
    mutable n_frags: uint64;
    mutable n_hits: uint64;
    mutable n_missed: uint64;
    mutable n_lost: uint64;
  }

  type lookup_ret = 
         Found of Entry.t ref
       | NOT_FOUND

  type t = {
    (* Mapping Netif objects to ports *)
    mutable dev_to_port: (OS.Netif.id, port ref) Hashtbl.t;

    (* Mapping port ids to port numbers *)
    mutable int_to_port: (int, port ref) Hashtbl.t;
    mutable ports : port list;
    mutable controllers: (Net.Channel.t) list;
    table: Table.t;
    stats: stats;
    p_sflow: uint32; (** probability for sFlow sampling *)
    mutable errornum : uint32; 
    mutable portnum : int;
    packet_queue : (Cstruct.buf * OS.Netif.id) Lwt_stream.t;
    push_packet : ((Cstruct.buf * OS.Netif.id) option -> unit);
    mutable queue_len : int;
    features : OP.Switch.features;
  }
  let init_port_counters port_id =
    OP.Port.({ port_id;
    rx_packets=0L; tx_packets=0L; rx_bytes=0L; tx_bytes=0L;
    rx_dropped=0L; tx_dropped=0L; rx_errors=0L; tx_errors=0L;
    rx_frame_err=0L; rx_over_err=0L;
    rx_crc_err=0L; collisions=0L;})

  let update_port_tx_stats pkt_len port = 
    port.counter.OP.Port.tx_packets <- (Int64.add 
      port.counter.OP.Port.tx_packets 1L);
    port.counter.OP.Port.tx_bytes <- (Int64.add 
      port.counter.OP.Port.tx_bytes pkt_len)

  let update_port_rx_stats pkt_len port = 
    port.counter.OP.Port.rx_packets <- (Int64.add 
      port.counter.OP.Port.rx_packets 1L);
    port.counter.OP.Port.rx_bytes <- (Int64.add 
      port.counter.OP.Port.rx_bytes pkt_len)

  let forward_frame st in_port frame pkt_size = function
    | OP.Port.Port(port) -> 
      if Hashtbl.mem st.int_to_port port then(
        let out_p = (!( Hashtbl.find st.int_to_port port))  in
          OS.Netif.write out_p.ethif frame )
      else
        return (Printf.printf "Port %d not registered \n" port)
    | OP.Port.No_port -> return ()
    | OP.Port.Flood 
    |OP.Port.All ->
      Lwt_list.iter_p  
      (fun port -> 
        if(port.port_id != (OP.Port.int_of_port in_port)) then (
          update_port_tx_stats (Int64.of_int (Cstruct.len frame)) port;
          OS.Netif.write port.ethif frame
        ) else
          return ()
      ) st.ports
    | OP.Port.In_port ->
      let port = (OP.Port.int_of_port in_port) in 
      if Hashtbl.mem st.int_to_port port then
        let out_p = !(Hashtbl.find st.int_to_port port) in
          update_port_tx_stats (Int64.of_int (Cstruct.len frame)) out_p;
          OS.Netif.write out_p.ethif frame
      else
        return (Printf.printf "Port %d not registered \n" port)
        (*           | Table
         *           | Normal
         *           | Controller -> generate a packet out. 
         *           | Local -> can I inject this frame to the network
         *           stack?  *)
        | _ -> return (Printf.printf "Not implemented output port\n")

  cstruct dl_header {
    uint8_t   dl_dst[6];
    uint8_t   dl_src[6]; 
    uint16_t  dl_type 
  } as big_endian

  cstruct arphdr {
    uint16_t ar_hrd;         
    uint16_t ar_pro;         
    uint16_t ar_hln;              
    uint16_t ar_pln;              
    uint16_t ar_op;          
    uint8_t ar_sha[6];  
    uint32_t nw_src;
    uint8_t ar_tha[6];  
    uint32_t nw_dst 
  } as big_endian

  cstruct nw_header {
    uint8_t        hlen_version;
    uint8_t        nw_tos;
    uint8_t        pad[7];
    uint8_t        nw_proto; 
    uint16_t       csum;
    uint32_t       nw_src; 
    uint32_t       nw_dst
  } as big_endian 

  cstruct tp_header {
    uint16_t tp_src;
    uint16_t tp_dst
  } as big_endian 

  cstruct icmphdr {
    uint8_t typ;
    uint8_t code;
    uint16_t checksum
  } as big_endian


  (* Assumwe that action are valid. I will not get a flow that sets an ip
   * address unless it defines that the ethType is ip. Need to enforce
   * these rule in the parsing process of the flow_mod packets *)
  let apply_of_actions st in_port bits actions =
    let apply_of_actions_inner st in_port bits = function
      | OP.Flow.Output (port, pkt_size) ->
        (* Make a packet copy in case the buffer is modified and multiple
         * outputs are defined? *)
        forward_frame st in_port bits pkt_size port
      | OP.Flow.Set_dl_src(eaddr) ->
        return (set_dl_header_dl_src eaddr 6 bits)
      | OP.Flow.Set_dl_dst(eaddr) ->
        return (set_dl_header_dl_dst eaddr 6 bits)
        (* TODO: Add for this actions to check when inserted if 
          * the flow is an ip flow *)
      | OP.Flow.Set_nw_tos(tos) -> 
        let ip_data = Cstruct.shift bits sizeof_dl_header in
          return (set_nw_header_nw_tos ip_data (int_of_char tos))
      (* TODO: wHAT ABOUT ARP?
       * *)
      | OP.Flow.Set_nw_src(ip) -> 
        let ip_data = Cstruct.shift bits sizeof_dl_header in
          return (set_nw_header_nw_src ip_data ip)
      | OP.Flow.Set_nw_dst(ip) -> 
        let ip_data = Cstruct.shift bits sizeof_dl_header in
          return (set_nw_header_nw_dst ip_data ip)
      | OP.Flow.Set_tp_src(port) ->
        let ip_data = Cstruct.shift bits sizeof_dl_header in
        let len = (get_nw_header_hlen_version bits) land 0xf in 
        let tp_data = Cstruct.shift ip_data (len*4) in 
          return (set_tp_header_tp_src tp_data port)
      | OP.Flow.Set_tp_dst(port) ->
        let ip_data = Cstruct.shift bits sizeof_dl_header in
        let len = (get_nw_header_hlen_version bits) land 0xf in 
        let tp_data = Cstruct.shift ip_data (len*4) in 
          return (set_tp_header_tp_dst tp_data port )
      | OP.Flow.Enqueue(_, _)
      | OP.Flow.Set_vlan_pcp _
      | OP.Flow.Set_vlan_vid _
      | OP.Flow.VENDOR_ACT 
      | OP.Flow.STRIP_VLAN ->
        (* VLAN manupulation actions *)
        return (pr "Unsupported action STRIP_VLAN\n")
    in
    let rec apply_of_actions_rec st in_port actions = function
      | [] -> return ()
      | head :: actions -> 
        let _ = apply_of_actions_inner st in_port bits head in
          apply_of_actions_rec st in_port bits actions 
    in 
      apply_of_actions_rec st in_port bits actions


   let errornum = ref 1 
   let lookup_flow st of_match =
     (* Check first the match table cache *)
     if (Hashtbl.mem st.table.Table.cache of_match ) then (
       let entry = (Hashtbl.find st.table.Table.cache of_match) in
       Found(entry) 
     ) else (
       (* Check the wilcard card table *)
       let lookup_flow flow entry ret =
         if (OP.Match.flow_match_compare of_match flow
                flow.OP.Match.wildcards) then
           ret @ [flow]
         else
           ret
       in
       let matches = Hashtbl.fold lookup_flow st.table.Table.entries [] in
       if ((List.length matches) = 0) then 
         NOT_FOUND
         else (
           (* TODO choose highest priority on the list instead of picking the 
            * first entry *)
           let flow_ref = Hashtbl.find st.table.Table.entries
                            (List.hd matches) in 
           Hashtbl.add st.table.Table.cache of_match (ref flow_ref);
           Found((ref flow_ref))
           )
         )
end

let st = 
  let (packet_queue, push_packet) = Lwt_stream.create () in
  let capabilities = OP.Switch.(
    { flow_stats=true; table_stats=true; port_stats=true; stp=true; 
    ip_reasm=false; queue_stats=false; arp_match_ip=true; }) in 
  let actions = OP.Switch.({
    output=true; set_vlan_id=true; set_vlan_pcp=true; strip_vlan=true;
    set_dl_src=true; set_dl_dst=true; set_nw_src=true; set_nw_dst=true;
    set_nw_tos=true; set_tp_src=true; set_tp_dst=true; enqueue=true;
    vendor=true; }) in 
  let features = OP.Switch.(
    {datapath_id=1L; n_buffers=0l; n_tables=(char_of_int 1); 
    capabilities; actions; ports=[];}) in
  Switch.(
    { ports = []; int_to_port = (Hashtbl.create 0);
    dev_to_port=(Hashtbl.create 64); 
    table = Table.(
      { tid = 0_L; entries = (Hashtbl.create 10000); cache = (Hashtbl.create 10000);
        stats = OP.Stats.(
          {table_id=(OP.Stats.table_id_of_int 1); name="main_tbl"; 
          wildcards=OP.Wildcards.exact_match; max_entries=1024l; active_count=0l; 
          lookup_count=0L; matched_count=0L});});
      p_sflow = 0_l; controllers=[]; errornum = 0l;
      portnum=0; packet_queue; push_packet; queue_len = 0;
      stats={n_frags=0L; n_hits=0L;n_missed=0L;n_lost=0L;}; features; })

let add_flow tuple actions =
  (* TODO check if the details are correct e.g. IP type etc. *)
  Hashtbl.replace st.Switch.table.Table.entries tuple
  (Entry.({actions; counters=(init_flow_counters ()); cache_entries=[];}))

let del_flow tuple out_port =
  (* Delete all matching entries from the flow table*)
  let remove_flow = 
    Hashtbl.fold (
      fun of_match flow ret -> 
        if (OP.Match.flow_match_compare of_match tuple
        tuple.OP.Match.wildcards) then ( 
          Hashtbl.remove st.Switch.table.Table.entries of_match; 
          ret @ [flow]
        ) else 
          ret
    ) st.Switch.table.Table.entries [] in

  (* Delete all entries from cache *)
  Hashtbl.iter (fun of_match flow -> 
    if (List.mem (!flow) remove_flow ) then 
      Hashtbl.remove st.Switch.table.Table.cache of_match
    ) st.Switch.table.Table.cache

let pkt_count = ref 0L
let process_frame intf_name frame = 
    if ((Hashtbl.mem st.Switch.dev_to_port intf_name) 
        && (st.Switch.queue_len < 256)) then (
(*       let p = (!(Hashtbl.find st.Switch.ports intf_name)) in *)
(*       let in_port = (OP.Port.port_of_int p.Switch.port_id) in *)
      st.Switch.queue_len <- st.Switch.queue_len + 1;
      return(st.Switch.push_packet (Some(frame, intf_name)))
    ) else (
      return ()
    )

(* 
 * let process_frame_depr intf_name frame =  *)
let process_frame_inner intf_name frame =
  try_lwt 
     let p = (!(Hashtbl.find st.Switch.dev_to_port intf_name)) in 
     pkt_count := (Int64.add !pkt_count 1L);
     let in_port = (OP.Port.port_of_int p.Switch.port_id) in 
     let tupple = (OP.Match.raw_packet_to_match in_port frame ) in
     (* Update port rx statistics *)
     let _ = Switch.update_port_rx_stats (Int64.of_int (Cstruct.len frame)) p in

     (* Lookup packet flow to existing flows in table *)
     let entry = (Switch.lookup_flow st tupple) in 
     match entry with 
     | Switch.NOT_FOUND ->
       let _ = Table.update_table_missed st.Switch.table in
       let bits = OP.marshal_and_sub (OP.Packet_in.marshal_pkt_in  ~port:in_port
       ~reason:OP.Packet_in.NO_MATCH frame) (OS.Io_page.get ()) in 
       Lwt_list.iter_s 
       (fun t -> 
         let _ = Channel.write_buffer t bits in 
          Channel.flush t)
       st.Switch.controllers
       (* generate a packet in event *)
     | Switch.Found(entry) ->
       let _ = Table.update_table_found st.Switch.table in
       let _ = Entry.update_flow (Int64.of_int (Cstruct.len frame)) !entry in
        Switch.apply_of_actions st tupple.OP.Match.in_port 
          frame (!entry).Entry.actions
    with Not_found ->
      return (pr "Port %s not found\n%!" intf_name) 

let proccess_packets () = 
  try_lwt 
  while_lwt true do 
    lwt a = Lwt_stream.get st.Switch.packet_queue in
    match a with
    | Some (port, pkt) ->
      st.Switch.queue_len <- st.Switch.queue_len - 1;
      process_frame_inner pkt port
    | None -> return ()
  done

let add_port sw mgr ethif = 
  st.Switch.portnum <- st.Switch.portnum + 1;
  Printf.printf "Adding port %d (%s)\n %!" st.Switch.portnum ethif.OS.Netif.id;
  let features = OP.Port.({pause_asym=true; pause=true; autoneg=true; fiber=true;
        copper=true; f_10GB_FD=true; f_1GB_FD=true; f_1GB_HD=true; f_100MB_FD=true;
        f_100MB_HD=true; f_10MB_FD=true; f_10MB_HD=true;}) in
  let phy= OP.Port.({port_no=(st.Switch.portnum); hw_addr=(ethif.OS.Netif.mac);
        name=ethif.OS.Netif.id; 
        config = 
          { port_down=false; no_stp=false; no_recv=false; no_recv_stp=false; 
          no_flood=false; no_fwd=false; no_packet_in=false;};
        state= 
          {link_down =false; stp_listen =false; stp_learn =false;
          stp_forward =false; stp_block =false;};
        curr=features; advertised=features; supported=features; 
        peer=features;}) in
  let port =  Switch.(
      {port_id= st.Switch.portnum; mgr; port_name=ethif.OS.Netif.id;
      counter=(Switch.init_port_counters st.Switch.portnum); ethif;}) in  
  sw.Switch.ports <- sw.Switch.ports @ [port];
    Hashtbl.add sw.Switch.int_to_port st.Switch.portnum (ref port); 
    Hashtbl.add sw.Switch.dev_to_port ethif.OS.Netif.id (ref port);
    sw.Switch.features.OP.Switch.ports  <- 
      sw.Switch.features.OP.Switch.ports @ [phy]

type endhost = {
  ip: Nettypes.ipv4_addr;
  port: int;
}

let process_openflow  state t bits =  function
  | OP.Hello (h) -> 
    (* Reply to HELLO with a HELLO and a feature request *)
    cp "HELLO";
    let bits = OP.marshal_and_sub (OP.Header.marshal_header h)
      (OS.Io_page.get ()) in 
    let _ = Channel.write_buffer t bits in 
(*     (OP.build_features_req 0_l)  *)
      Channel.flush t
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
      (OP.Switch.marshal_reply_features h.OP.Header.xid st.Switch.features )
      (OS.Io_page.get ()) in 
    let _ = Channel.write_buffer  t bits in
      Channel.flush t
  | OP.Stats_req(h, req) -> begin
    let xid = h.OP.Header.xid in 
    cp "STATS_REQ\n%!";
    match req with
    | OP.Stats.Desc_req(req) ->
      let desc = OP.Stats.({ imfr_desc="Mirage"; hw_desc="Mirage";
                  sw_desc="Mirage"; serial_num="0.1";dp_desc="Mirage";}) 
      in  
      let resp_h = OP.Stats.({st_ty=DESC; more_to_follow=false;}) in 
      let bits =  OP.marshal_and_sub 
                  (OP.Stats.marshal_stats_resp xid (OP.Stats.Desc_resp(resp_h,
                  desc))) 
                  (OS.Io_page.get ()) in   
      let _ = Channel.write_buffer t bits in
        Channel.flush t 
    | OP.Stats.Flow_req(req_h, of_match, table_id, out_port) ->
      (*TODO Need to consider the  table_id and the out_port and 
       * split reply over multiple openflow packets if they don't
       * fit a single packet. *)
      let match_flows of_match key value ret =
        if (OP.Match.flow_match_compare key of_match 
              of_match.OP.Match.wildcards) then 
                ret @ 
                [(Entry.flow_counters_to_flow_stats of_match (char_of_int 1)
                value)] 
        else 
          ret 
      in
      let flows = 
        Hashtbl.fold (fun key value r -> match_flows of_match key value r) 
          st.Switch.table.Table.entries [] in 
      let stats = OP.Stats.({st_ty=FLOW; more_to_follow=false;}) in 
      let reply = OP.Stats.Flow_resp(stats, flows) in 
      let bits = OP.marshal_and_sub (OP.Stats.marshal_stats_resp xid reply) 
                  (OS.Io_page.get ()) in
      let _ = Channel.write_buffer t bits in
        Channel.flush t
    | OP.Stats.Aggregate_req (req_h, of_match, table, port) -> 
      let aggr_flow_bytes = ref 0L in
      let aggr_flow_pkts = ref 0L in
      let aggr_flows = ref 0l in            
      let match_flows_aggr of_match key value =
        if (OP.Match.flow_match_compare key of_match 
              of_match.OP.Match.wildcards) then (
          aggr_flows := Int32.add (!aggr_flows) 1l;
          aggr_flow_bytes := Int64.add (!aggr_flow_bytes) 
                              value.Entry.counters.Entry.n_bytes; 
          aggr_flow_pkts := Int64.add (!aggr_flow_pkts)
                         value.Entry.counters.Entry.n_packets
          ) in 
      Hashtbl.iter (fun key value -> match_flows_aggr of_match key value)
                    st.Switch.table.Table.entries;
      let stats = OP.Stats.({st_ty=AGGREGATE; more_to_follow=false;}) in  
      let reply = OP.Stats.Aggregate_resp(stats, 
                    OP.Stats.({byte_count=(!aggr_flow_bytes);
                    packet_count=(!aggr_flow_pkts);
                    flow_count=(!aggr_flows);})) 
      in 
      let bits = OP.marshal_and_sub (OP.Stats.marshal_stats_resp xid reply)
                  (OS.Io_page.get ()) in
      let _ =  Channel.write_buffer t bits in 
        Channel.flush t
    | OP.Stats.Table_req(req) ->
      let stats = OP.Stats.({st_ty=TABLE; more_to_follow=false;}) in  
      let reply = OP.Stats.Table_resp(stats, [st.Switch.table.Table.stats]) in 
      let bits = OP.marshal_and_sub (OP.Stats.marshal_stats_resp xid reply) 
                  (OS.Io_page.get ()) in
      let _ = Channel.write_buffer t bits in 
        Channel.flush t
    | OP.Stats.Port_req(req_h, port) -> begin
      match port with
      | OP.Port.No_port -> 
        let port_stats = List.map (fun p -> p.Switch.counter) st.Switch.ports in
        let stats = OP.Stats.({st_ty=PORT; more_to_follow=false;}) in 
        let reply = OP.Stats.Port_resp(stats, port_stats) in 
        let bits = OP.marshal_and_sub (OP.Stats.marshal_stats_resp xid reply) 
                    (OS.Io_page.get ()) in
        let _ = Channel.write_buffer t bits in 
          Channel.flush t
      | OP.Port.Port(port_id) -> 
        try_lwt 
          let port = Hashtbl.find st.Switch.int_to_port port_id in
          let stats = OP.Stats.({st_ty=PORT; more_to_follow=false;}) in 
          let reply = OP.Stats.Port_resp(stats, [(!port).Switch.counter]) in 
          let bits = OP.marshal_and_sub (OP.Stats.marshal_stats_resp xid reply) 
                      (OS.Io_page.get ()) in
          let _ = Channel.write_buffer t bits in 
            Channel.flush t
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
    let resp = OP.Switch.init_switch_config in
    let bits = OP.marshal_and_sub (OP.Switch.marshal_switch_config 
                h.OP.Header.xid resp) (OS.Io_page.get ()) in
    let _ = Channel.write_buffer t bits in 
      Channel.flush t
  | OP.Barrier_req(h) ->
    let resp_h = (OP.Header.create OP.Header.BARRIER_RESP
                  (OP.Header.sizeof_ofp_header) h.OP.Header.xid) in
    let bits = OP.marshal_and_sub (OP.Header.marshal_header resp_h)        
                (OS.Io_page.get ()) in
    let _ = Channel.write_buffer t bits in 
      Channel.flush t
  | OP.Packet_out(h, pkt) ->
    Printf.printf "packet out received with actions : %s \n%!"
    (OP.Flow.string_of_actions pkt.OP.Packet_out.actions);
    Switch.apply_of_actions st pkt.OP.Packet_out.in_port
              pkt.OP.Packet_out.data pkt.OP.Packet_out.actions
  | OP.Flow_mod(h,fm)  ->
    let of_match = fm.OP.Flow_mod.of_match in 
    let of_actions = fm.OP.Flow_mod.actions in
    (*Printf.printf "need to insert rule %s actions %s\n%!" 
    (OP.Match.match_to_string of_match) 
               (OP.Flow.string_of_actions fm.OP.Flow_mod.actions));
 *)
    let _ = 
      match (fm.OP.Flow_mod.command) with
      | OP.Flow_mod.ADD 
      | OP.Flow_mod.MODIFY 
      | OP.Flow_mod.MODIFY_STRICT -> 
        add_flow of_match of_actions
      | OP.Flow_mod.DELETE 
      | OP.Flow_mod.DELETE_STRICT ->
        del_flow of_match fm.OP.Flow_mod.out_port
    in
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

let rec read_cache_data t data_cache len = 
(*     Printf.printf "let rec read_cache_data t data_cache %d = \n%!" len; *)
    match (len, !data_cache) with
    | (0, _) -> 
(*       pp "| (0, _) ->\n%!"; *)
      return (Cstruct.sub (OS.Io_page.get ()) 0 0 )
    | (_, []) ->
(*       pp " | (_, []) ->\n%!"; *)
      lwt data = Channel.read_some t in
        data_cache := [data];
        read_cache_data t data_cache len
    | (_, [head]) when ((Cstruct.len head) = len) ->
(*       pp "| (_, [head]) when ((Cstruct.len head) = len) -> \n%!"; *)
      data_cache := [];
      return head
    | (_, [head]) when ((Cstruct.len head) < len) ->
(*       pp "| (_, [head]) when ((Cstruct.len head) < len) -> \n%!"; *)
      lwt data = (Channel.read_some t) in
      data_cache := [head; data];
      read_cache_data t data_cache len
    | (_, [head]) when ((Cstruct.len head) > len) -> (
(*       pp "| (_, [head]) when ((Cstruct.len head) > len) ->\n%!"; *)
      data_cache := [(Cstruct.shift head len)];
      return (Cstruct.sub head 0 len) )
    | (_, head::tail) when ((Cstruct.len head) = len) -> (
(*       pp "| (_, head::tail) when ((Cstruct.len head) = len) ->\n%!"; *)
      data_cache := tail;
      return head)
    | (_, head::tail) when ((Cstruct.len head) > len) ->
(*       pp "| (_, head::tail) when ((Cstruct.len head) > len) ->\n%!"; *)
      data_cache := [(Cstruct.shift head len)] @ tail;
      return (Cstruct.sub head 0 len) 
    | (_, head::head_2::tail) when ((Cstruct.len head) > len) -> (
(*       pp "| (_, head::head_2::tail) when ((Cstruct.len head) > len) ->\n%!";
 *       *)
      let head_len = Cstruct.len head in 
      let buf = Cstruct.sub_buffer (OS.Io_page.get ()) 0 len in 
      let _ = Cstruct.blit_buffer head 0 buf 0 head_len in
      (* Maybe  -1  here? *)
      let _ = Cstruct.blit_buffer buf (head_len - 1) head_2 0 (len -head_len) in 
      let head_2 = Cstruct.shift head_2 (len -head_len) in
      data_cache := [head_2] @ tail;
      return buf)
    | (_, _) ->
(*       pp "| (_, _) ->\n%!"; *)
      Printf.printf "read_cache_data and not match found\n%!";
      return (Cstruct.sub (OS.Io_page.get ()) 0 0 )

let controller (remote_addr, remote_port) t =
  let rs = Nettypes.ipv4_addr_to_string remote_addr in
  Log.info "OpenFlow Controller" "+ %s:%d" rs remote_port; 
  st.Switch.controllers <- (st.Switch.controllers @ [t]);
  let data_cache = ref [] in 
  let rec echo () =
    try_lwt
      lwt hbuf = read_cache_data t data_cache OP.Header.sizeof_ofp_header in
        let ofh  = OP.Header.parse_header  hbuf in
        let dlen = ofh.OP.Header.len - OP.Header.sizeof_ofp_header in 
        lwt dbuf = read_cache_data t data_cache dlen in
        let ofp  = OP.parse ofh dbuf in
          process_openflow st t dbuf ofp (* Bitstring.concat [hbuf; dbuf] *) 
          >> echo ()
    with
    | Nettypes.Closed -> 
            (* TODO Need to remove the t from st.Switch.controllers *)
      return ()
    | OP.Unparsed (m, bs) -> (pr "# unparsed! m=%s\n %!" m); echo ()

  in 
    echo () 

(*let get_len_data data_cache len = 
  bitmatch (!data_cache) with
    | {ret:len*8:bitstring; buff:-1:bitstring} ->
      (data_cache := buff; 
      return ret)


let read_cache_data t data_cache len = 
  if ((Bitstring.bitstring_length (!data_cache)) < (len*8)) then (
    lwt buf = (Channel.read_some t) in
      data_cache := (Bitstring.concat [(!data_cache); buf; ]);
      get_len_data data_cache len
  ) else 
    get_len_data data_cache len

let controller_connect t =
  st.Switch.controllers <- (st.Switch.controllers @ [t]);
    let data_cache = ref (Bitstring.empty_bitstring) in 
    let rec echo () =
      try_lwt
        lwt hbuf = read_cache_data t data_cache (OP.Header.get_len ) in
        let ofh  = OP.Header.parse_h hbuf in
        let dlen = ofh.OP.Header.len - OP.Header.get_len in 
        (* lwt dbuf = rd_data dlen t in *)
        lwt dbuf = read_cache_data t data_cache dlen in 
        let ofp  = OP.parse ofh dbuf in
        process_of_packet st ofp t 
        >> echo ()
      with
        | Nettypes.Closed -> 
            (* TODO Need to remove the t from st.Switch.controllers *)
            return ()
        | OP.Unparsed (m, bs) -> (pr "# unparsed! m=%s\n %!" m); echo ()

    in echo () *)
   
let listen mgr loc init =
  init mgr st; 
  Channel.listen mgr (`TCPv4 (loc, controller)) <&> (proccess_packets ())

(* let connect mgr loc init =
  init mgr st; 
  Channel.connect mgr (`TCPv4 (loc, controller_connect)) *)

