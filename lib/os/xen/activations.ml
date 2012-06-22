(*
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
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

external evtchn_init: unit -> unit = "caml_evtchn_init"
external evtchn_nr_events: unit -> int = "caml_nr_events"
external evtchn_test_and_clear: int -> bool = "caml_evtchn_test_and_clear" "noalloc"

let _ = evtchn_init ()
let nr_events = evtchn_nr_events ()
let event_cb = Array.init nr_events (fun _ -> Lwt_sequence.create ())

let timeout = 0.5

let event = ref 0

(* Block waiting for an event to occur on a particular port *)
let wait port =
  let th, u = Lwt.task () in 
(*   let u = Lwt_mvar.create_empty () in  *)
(*
  let _ = 
    if (port = 4) then 
      event := !event + 1
  in
*)
  let node = Lwt_sequence.add_r (u,!event) event_cb.(port) in
(*
  let _ = 
    if (port = 4) then
  Printf.printf "1111111 %d waiting on port %d\n%!" !event port
  in
*)
  Lwt.on_cancel th (fun _ -> Lwt_sequence.remove node); 
(*
  lwt _ = Lwt.choose [(Lwt_mvar.take u); (Time.sleep timeout);] in 
  Lwt_sequence.remove node;
  Lwt.return ()
*)
  lwt _ = th  in 
  Lwt.return( 
(*
    if (port = 4) then
      Printf.printf "5555555 %d fired on port %d\n%!" !event port
*)
    )

  let last_event = ref 0
(* Go through the event mask and activate any events, potentially spawning
   new threads *)
let run () =
  for port = 0 to nr_events - 1 do
    (* XXX workaround rare event wedge bug XXX *)
    if true || evtchn_test_and_clear port then begin
      Lwt_sequence.iter_node_l (fun node ->
        let (u, event) = Lwt_sequence.get node in
        Lwt_sequence.remove node;
(*
        let _ = if ( port = 4 ) then 
        Printf.printf "3333333 %d firing on port %d\n%!" event port in 
*)

        let _ = 
(*          if ( port = 4 ) then
            Lwt.wakeup_debug u () 
          else 
*) 
        Lwt.wakeup u ()
        in 
(*
        let _ = if ( port = 4 ) then 
        Printf.printf "4444444 %d firing on port %d\n%!" event port in 
*)
        ()

(*         Lwt.ignore_result(Lwt_mvar.put  u ()) *)
      ) event_cb.(port)
    end
    (*
     else (
      if ((port = 4) & (!last_event <> !event)) then (
        last_event := !event;
        Printf.printf "2222222 %d No event found for port %d\n%!" !event port)
    )
*)
  done
