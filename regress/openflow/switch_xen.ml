(* 
 * Copyright (c) 2011 Charalampos Rotsos <cr409@cl.cam.ac.uk>
 * Copyright (c) 2011 Richard Mortier <mort@cantab.net>
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
open OS.Time

let resolve t = Lwt.on_success t (fun _ -> ())

module OP = Openflow.Ofpacket
(* module OS = Openflow.Switch *)
(* module OE = OS.Event *)

let pp = Printf.printf
let sp = Printf.sprintf

type switch_state = {
  mutable sw : Openflow.Switch.Switch.t list;
  mutable of_port : (Net.Manager.t * Net.Manager.interface) list;
}

let sw_details = {sw=[]; of_port=[];}

let init controller sw =
  sw_details.sw <- ([sw] @ sw_details.sw);
  return ()


let ip = Net.Nettypes.(
  (ipv4_addr_of_tuple (10l,0l,0l,3l),
   ipv4_addr_of_tuple (255l,255l,255l,0l),
   [ ipv4_addr_of_tuple (10l,0l,0l,2l) ]
  )) 

let main () =

  Log.info "OF switch" "starting switch";
  lwt _ = Net.Manager.create_xen_switch (fun mgr interface id ->
    if ((int_of_string id) == 0) then (
      Net.Manager.configure interface (`IPv4 ip);
      let port = 6633 in 
      Openflow.Switch.listen mgr (None, port) init
        >> return (Log.info "OF Switch" "done!")
    ) else (
      List.iter  ( fun sw -> 
        Openflow.Switch.add_port sw mgr interface) sw_details.sw; 
        return (Printf.printf "this is not the listening port %s\n%!" id)
    )
  ) in 
    return () 
