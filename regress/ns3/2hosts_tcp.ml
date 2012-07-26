open Lwt
open OS
open Net
open Printf

let port = 55555
let use_dhcp = false

let iter (nm,sz,m) =
  Console.log ("start: " ^ nm);
  let tot = ref 0. in
  for_lwt i = 1 to sz do
    Console.log (Printf.sprintf "%s %d" nm i);
    let t = float_of_int i /. m in
    tot := t +. !tot;
    Time.sleep t;
  done >>
    (Console.log ("done: " ^ nm); return ())

let rec echo dst chan = 
(*   Log.info "Channel_echo" "callback!"; *)
  try_lwt
  lwt _ =
    while_lwt true do
      lwt buf = Channel.read_some chan in
(*         return () *)
(*
       return (Printf.printf "%f: read %d\n%!" 
       (Clock.time ())
       (Cstruct.len buf)) 
*)
      return ()
    done
  in
    return ()
  with Nettypes.Closed -> return (Log.info "Echo" "closed!")

let rec echo_client chan = 
(*   Log.info "Channel_echo" "callback!";  *)
  printf "XXXXX echo test started \n%!";
  try_lwt
    let data = String.create 1460 in 
(*       while_lwt true do  *)
    let rec send_data () = 
        let _ = Channel.write_string chan data 0 (String.length data) in
        lwt _ = Time.sleep 0.001 in
(*         Printf.printf "%f: Writing new buffer....\n%!" (Clock.time ());  *)
        lwt _ = Channel.flush chan in
          send_data ()
    in
      send_data ()
(*       done   *)
  with 
    | Nettypes.Closed -> return (Log.info "Echo" "closed!")
    | ex ->  return (Printf.printf "Eroor:%s\n%!" (Printexc.to_string ex))

let rec echo_udp dst buf = 
(*   Log.info "Channel_echo" "callback!"; *)
(*
  Printf.printf "%f: read %d\n%!" 
    (Clock.time ())
    (Cstruct.len buf);
*)
    return () 
  
let rec echo_client_udp mgr dst = 
(*   Log.info "Channel_echo" "callback!";  *)
  printf "XXXXX echo test started \n%!";
  try_lwt
    let data = Io_page.create 1460 in
(*     let data = String.create 1460 in  *)
(*       while_lwt true do  *)
    let rec send_data () = 
        lwt _ = Net.Datagram.UDPv4.send mgr dst data in
        lwt () = Time.sleep 1.0 in
(*         Printf.printf "%f: Writing new buffer....\n%!" (Clock.time ());  *)
          send_data ()
    in
      send_data ()
(*       done   *)
  with 
    | Nettypes.Closed -> return (Log.info "Echo" "closed!")
    | ex ->  return (Printf.printf "Eroor:%s\n%!" (Printexc.to_string ex))


let count = ref 1

(* Code to run on the end node *)
let run_inner () =
  let node_id = !count in 
    count := !count + 1;
(*
  let t1 = "one", 5, 3.1 in
  let t2 = "two", 3, 1.9 in
  let t3 = "three", 4, 1.8 in
  let t4 = "four", 5, 3.2 in
  let r t = iter t >> return () in
*)
  let test = 
  try_lwt 
    Net.Manager.create (fun mgr interface id ->
(*       Printf.printf "XXXXXX Manager callback\n%!"; *)
      let ip node_id = 
        Net.Nettypes.(
        (ipv4_addr_of_tuple (10l,0l,0l,(Int32.of_int node_id)),
         ipv4_addr_of_tuple (255l,255l,255l,0l),
         [ ipv4_addr_of_tuple (10l,0l,0l,1l) ]
        )) in
          (
          match use_dhcp with
            | false -> (  
                match node_id with 
                  | 1 -> (
                    match id with
                    | "0" -> (
                      Printf.printf "Listening Server\n%!";
                      lwt _ = Manager.configure interface (`IPv4 (ip node_id)) in
                      lwt () = Net.Datagram.UDPv4.recv mgr (None, port) echo_udp in 
(*                      lwt () = Net.Channel.listen mgr 
                                 (`TCPv4 ((None, port), echo)) in *)
(*                         return (Log.info "Channel_echo" "done!"))  *)
                        return ())
                    | "1" ->
                       Printf.printf "External intf for node 1\n%!";
                       let ip = 
                         Net.Nettypes.(
                           (ipv4_addr_of_tuple (10l,0l,1l,2l),
                           ipv4_addr_of_tuple (255l,255l,255l,0l),
                           [ ipv4_addr_of_tuple (10l,0l,1l,1l) ]
                           )) in     
                        lwt _ = Manager.configure interface (`IPv4 ip) in
                        lwt () = Net.Channel.listen mgr 
                                   (`TCPv4 ((None, port), echo)) in 
(*                         return (Log.info "Channel_echo" "done!"))  *)
                          return () 
                    | _ -> return (eprintf "Invalid net id %s\n%!" id))
                  | _ -> (
                      let dst_ip = Net.Nettypes.ipv4_addr_of_tuple (10l,0l,0l,1l) in  
                      Printf.printf "%f: trying to connect client \n%!"
                        (Clock.time ());
                      lwt _ = Manager.configure interface (`IPv4 (ip node_id)) in
                      lwt _ = Time.sleep 1.0 in
                      Printf.printf "%f: trying to connect client \n%!"
                        (Clock.time ());
                      lwt () = echo_client_udp mgr (dst_ip,port) in
(*                      lwt () =  Net.Channel.connect mgr 
                                  (`TCPv4 (None, (dst_ip, port), echo_client))
                      in *)
                        return ())
              )
            | true -> Manager.configure interface (`DHCP)
      ))
    with e ->
     Printf.eprintf "Error: %s" (Printexc.to_string e); 
     return ()
(*                    in *)
  in  
(*   join [ test; r t1; r t2; r t3; r t4] *)
    test

(* Design the topology *)
let run () =
  Gc.set { (Gc.get()) with Gc.minor_heap_size = 512000000 };
  Gc.set { (Gc.get()) with Gc.major_heap_increment = 512000000 };
  Gc.set { (Gc.get()) with Gc.stack_limit = 512000000 };
  Gc.set { (Gc.get()) with Gc.allocation_policy = 0 };
  Gc.set { (Gc.get()) with Gc.space_overhead = 200 }; 
  OS.Topology.add_node "node1" run_inner;
  OS.Topology.add_node "node2" run_inner;
  OS.Topology.add_link "node1" "node2";
  OS.Topology.add_external_dev "nstap0" "node1" "10.0.1.0" "255.255.255.0"
