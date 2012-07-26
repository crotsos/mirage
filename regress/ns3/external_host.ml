open Lwt
open OS
open Net
open Printf

let port = 55555

let rec echo dst chan = 
  try_lwt
  lwt _ =
    while_lwt true do
      lwt buf = Channel.read_some chan in
      return ()
    done
  in
    return ()
  with Nettypes.Closed -> return (Log.info "Echo" "closed!")

(* Code to run on the end node *)
let run_inner () =
  try_lwt 
    Net.Manager.create (fun mgr interface id ->
      Printf.printf "External intf for node 1\n%!";
      let ip = 
        Net.Nettypes.(
          (ipv4_addr_of_tuple (10l,0l,1l,2l),
          ipv4_addr_of_tuple (255l,255l,255l,0l),
          [ ipv4_addr_of_tuple (10l,0l,1l,1l) ]
          )) in     
      lwt _ = Manager.configure interface (`IPv4 ip) in
      let _ = printf "Intf configured...\n%!" in
      lwt () = Net.Channel.listen mgr 
      (`TCPv4 ((None, port), echo)) in 
      return ()
      ) 
    with e ->
     Printf.eprintf "Error: %s" (Printexc.to_string e); 
     return ()

(* Design the topology *)
let run () =
  Time.set_duration 60;
  OS.Topology.add_node "node1" run_inner;
  OS.Topology.add_external_dev "nstap0" "node1" "10.0.1.1" "255.255.255.0"
