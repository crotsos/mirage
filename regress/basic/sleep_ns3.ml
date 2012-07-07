open Lwt
open OS
open Net

let port = 55555
let use_dhcp = false
let ip = Net.Nettypes.(
  (ipv4_addr_of_tuple (10l,0l,0l,2l),
   ipv4_addr_of_tuple (255l,255l,255l,0l),
   [ ipv4_addr_of_tuple (10l,0l,0l,1l) ]
  ))


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
  Log.info "Channel_echo" "callback!";
  try_lwt
    lwt bufs = Channel.read_line chan in
    List.iter (Channel.write_buffer chan) bufs;
    Log.info "Echo" "buf:%s" "";
    Channel.write_char chan '\n';
    lwt () = Channel.flush chan in
    echo dst chan
  with Nettypes.Closed -> return (Log.info "Echo" "closed!")


(* Code to run on the end node *)
let run_inner () =
  let t1 = "one", 5, 3.1 in
  let t2 = "two", 3, 1.9 in
  let t3 = "three", 4, 1.8 in
  let t4 = "four", 5, 3.2 in
  let r t = iter t >> return () in
  let test = 
    Net.Manager.create (fun mgr interface id ->
      Printf.printf "XXXXXX Manager callback\n%!";
      lwt () = (match use_dhcp with
        | false -> Manager.configure interface (`IPv4 ip)
        | true -> Manager.configure interface (`DHCP)
      ) in
      lwt () = Net.Channel.listen mgr (`TCPv4 ((None, port), echo)) in
      return (Log.info "Channel_echo" "done!")) 
  in  
  join [ test; r t1; r t2; r t3; r t4]

(* Design the topology *)
let run () =
  OS.Topology.add_node "node1" run_inner;
  OS.Topology.add_node "node2" run_inner;
  OS.Topology.add_link "node1" "node2"
