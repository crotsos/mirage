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

open Net
open Net.Nettypes
open Lwt 
open Printf 

(*
 * Traffic generation state description
 * *)

type state_t = {                                         
    sinme: ipv4_dst;
    sinhim: ipv4_dst;
    mutable is_open : bool;
    mutable tx_target: int32;        (* used by tx side *)
    mutable tx_pkts: int32;
    mutable tx_sent: int32;
    mutable tx_sent_cpt: int32;     (* bytes rx'd since checkpoint *)
    mutable tx_start: float;        (* send_data(): just as start sending *)
    mutable tx_stop: float;         (* send_data(): when all is sent *)
                                                                         
    mutable rx_pkts: int32;
    mutable rx_rcvd: int32;         (* used by rx side *)
    mutable rx_rcvd_cpt: int32;     (* bytes rx'd since checkpoint *)
    mutable rx_start: float;        (* sink_data(): when first bits rcvd *)
    mutable rx_stop: float;         (* sink_data(): when no bits rcvd *)
                                                                         
    (* state for handling more complex traffic generators *)
    mutable client_id: int;
    mutable object_count: int;
    mutable next_start_time: float; (* time until next wake up *)
}                                                       

let init_channel_state_t sinme sinhim is_open client_id = 
  {sinme;  sinhim; is_open; tx_target=0l; tx_pkts=0l;
   tx_sent=0l; tx_sent_cpt=0l; tx_start=0.0; tx_stop=0.0;
   rx_pkts=0l; rx_rcvd=0l; rx_rcvd_cpt=0l; rx_start=0.0; rx_stop=0.0;
   client_id; object_count=0; next_start_time=0.0;}

type traffic_model = 
(*  | Simple_rx of num_ports * base_port *)
  | Simple_rx of int * int
(*   | Simple_tx of num_conn * bytes * dhost * num_ports * base_port *)
  | Simple_tx of int * int32 * ipv4_addr * int * int
(*   | Svr of num_ports * base_port  *)
  | Svr of int * int
(*   | Simple_clt of n, bytes, dhost, num_ports, base_port *)
  | Simple_clt of int * int32 * ipv4_addr * int * int
(*   | Cts_ctl of n, bytes, dhost, num_ports, base_port *)
  | Cts_ctl of int * int32 * ipv4_addr * int * int
(*   | Surge_client of n, dhost, num_ports, base_port *)
  | Surge_client of int * ipv4_addr * int * int 

type pttcp_t = {
  mutable state: state_t list; 
  mode : traffic_model;
  mutable max_id : int;
}

let init_pttcp_state_t mode = 
  { state=[]; mode; max_id=0;}

let add_pttcp_state st src_port dst_ip dst_port is_open = 
   let client_id = st.max_id in 
  let state = init_channel_state_t  ((ipv4_addr_of_uint32 0l, src_port)) 
                (dst_ip,dst_port) is_open  client_id in
      st.max_id <- st.max_id + 1;
      st.state <- [state] @ st.state; 
      state.rx_start <- (OS.Clock.time ());
      state 

let print_pttcp_state_rx r = 
  while_lwt true do 
    OS.Time.sleep 1.0 >>
      return ()
  done 

let create_listeners mgr st num_ports base_port cb = 
    let rec port_num_list = function 
      | num when (num = base_port) -> []
      | num -> [num] @ (port_num_list (num-1))
    in
    let ports = port_num_list (base_port + num_ports) in 
      (Lwt_list.iter_p (
        fun port -> 
          Net.Channel.listen mgr (`TCPv4 ((None, port), (cb port) )) 
      ) ports) <&> (print_pttcp_state_rx st)

(*
 * A method to process rx channel for simple server
* *)
let simple_server_rx st src_port (dst_ip, dst_port) t = 
  try_lwt
    let state = add_pttcp_state st src_port dst_ip dst_port true in 
      while_lwt true do 
        lwt buf = Channel.read_some ~len:4 t in 
        let tx_len = 
          List.fold_right 
            (fun p r ->
               let v = Int32.of_int (Cstruct.get_uint8 buf p) in 
                 Int32.add (Int32.shift_left r 8) v
            ) [0;1;2;3] 0l in 
        let _ = 
          if (state.tx_target <> state.tx_sent) then 
            eprintf "%03.6f: warning: premature request on %d\n%!" (OS.Clock.time ())
              state.client_id
        in 
        let _ =
          state.tx_target <- tx_len;
          state.tx_sent <- 0l;
          state.tx_pkts <- 0l
        in
        let rec send_data state t = function 
          | 0l -> return ()
          | len when (len > 1460l) -> 
            let buf = Cstruct.sub (OS.Io_page.get ()) 0 1460 in 
            let _ = Channel.write_buffer t buf in 
            lwt _ = Channel.flush t in 
              state.tx_sent <- Int32.add state.tx_sent 1460l;
              state.tx_pkts <- Int32.add state.tx_pkts 1l;
              send_data state t (Int32.sub len 1460l)
          | len ->
              let buf = Cstruct.sub (OS.Io_page.get ()) 0 (Int32.to_int len) in 
              let _ = Channel.write_buffer t buf in 
              lwt _ = Channel.flush t in 
                state.tx_sent <- Int32.add state.tx_sent len;
                state.tx_pkts <- Int32.add state.tx_pkts 1l;
                return 
                  (eprintf 
                     "%03.6f: flow %d - finished %ld bytes (%ld pkts)\n%!"
                     (OS.Clock.time ()) state.client_id state.tx_target 
                     state.tx_pkts)
        in 
          send_data state t tx_len 
      done
  with exn -> 
    return (eprintf "%03.f: simple_server_rx error : %s\n%!" (OS.Clock.time ()) 
              (Printexc.to_string exn))

let simple_rx st num_ports base_port = 
  return ()

let simple_tx st num_conn bytes dhost num_ports base_port = 
  return ()

let simple_client st num_conn bytes dhost num_ports base_port = 
  return ()

let continuous_client st num_conn bytes dhost num_ports base_port = 
  return ()

let surge_client st num_conn dhost num_ports base_port = 
  return ()


let generate_traffic mgr mode = 
  let st = init_pttcp_state_t mode in 
    match mode with 
      | Simple_rx(num_ports, base_port) -> 
          simple_rx st num_ports base_port 

      | Simple_tx(num_conn, bytes, dhost, num_ports, base_port) -> 
          simple_tx st num_conn bytes dhost num_ports base_port

      | Svr (num_ports, base_port ) -> 
          create_listeners mgr st num_ports base_port (simple_server_rx st)

      | Simple_clt(num_conn, bytes, dhost, num_ports, base_port) -> 
          simple_client st num_conn bytes dhost num_ports base_port

      | Cts_ctl (num_conn, bytes, dhost, num_ports, base_port) -> 
          continuous_client st num_conn bytes dhost num_ports base_port

      | Surge_client (num_conn, dhost, num_ports, base_port) ->
          surge_client st num_conn dhost num_ports base_port 
