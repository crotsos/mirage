(* 
 *  Copyright (c) 2012 Charalampos Rotsos <cr409@cl.cam.ac.uk>
 * 
 *  Permission to use, copy, modify, and distribute this software for any
 *  purpose with or without fee is hereby granted, provided that the above
 *  copyright notice and this permission notice appear in all copies.
 * 
 *  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 *  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 *  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 *  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 *  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 *  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 *  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt

external ns3_add_node : string -> string = "ocaml_ns3_add_node"
external ns3_add_link : string -> string -> unit = "ocaml_ns3_add_link"

type node_t = {
  name: string;
  cb_init : (unit -> unit Lwt.t);
}

type topo_t = {
  nodes : (string, node_t) Hashtbl.t;
}

let topo = 
  {nodes=(Hashtbl.create 64);}

let load t =
  Printf.printf "OS.Topology started...\n%!";
  let _ = t () in
  Main.run (return ())

let add_node name cb_init =
  let _ = ns3_add_node name in
    Hashtbl.replace topo.nodes name {name; cb_init;} 

let add_link node_a node_b =
  try 
    let _ = Hashtbl.find topo.nodes node_a in 
    let _ = Hashtbl.find topo.nodes node_b in 
      ns3_add_link node_a node_b 
  with Not_found -> ()

let node_name = Lwt.new_key ()

let exec fn () =
  Lwt.ignore_result (fn ())

let init_nodes () =
  Printf.printf "Initialising nodes....\n%!";
  Hashtbl.iter (
    fun _ node -> 
      Lwt.with_value node_name (Some(node.name)) (exec node.cb_init) ) topo.nodes

let _ = Callback.register "init" init_nodes

