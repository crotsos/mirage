(*
 * Copyright (c) 2010 Anil Madhavapeddy <anil@recoil.org>
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

type t = {
  tx_mss: int;
  mutable snd_una: Sequence.t;
  mutable tx_nxt: Sequence.t;
  mutable rx_nxt: Sequence.t;
  mutable rx_nxt_inseq: Sequence.t;
  mutable rx_nxt_lastack: Sequence.t;
  mutable tx_wnd: int32;           (* TX Window size after scaling *)
  mutable rx_wnd: int32;           (* RX Window size after scaling *)
  mutable tx_wnd_scale: int;       (* TX Window scaling option     *)
  mutable rx_wnd_scale: int;       (* RX Window scaling option     *)
  mutable ssthresh: int32;         (* threshold to switch from exponential
				      slow start to linear congestion
				      avoidance
				    *)
  mutable cwnd: int32;             (* congestion window *)
  mutable fast_recovery: bool;     (* flag to mark if this tcp is in
				      fast recovery *)
}

let default_mss = 536
let max_mss = 1460

(* To string for debugging *)
let to_string t =
  sprintf "rx_nxt=%s rx_nxt_inseq=%s tx_nxt=%s rx_wnd=%lu tx_wnd=%lu snd_una=%s"
    (Sequence.to_string t.rx_nxt)
    (Sequence.to_string t.rx_nxt_inseq)
    (Sequence.to_string t.tx_nxt)
    t.rx_wnd t.tx_wnd (Sequence.to_string t.snd_una)

(* Initialise the sequence space *)
let t ~rx_wnd_scale ~tx_wnd_scale ~rx_wnd ~tx_wnd ~rx_isn ~tx_mss =
  (* XXX need random ISN XXX *)
  let tx_nxt = Sequence.of_int32 7l in
  let rx_nxt = Sequence.(incr rx_isn) in
  let rx_nxt_inseq = Sequence.(incr rx_isn) in
  let rx_nxt_lastack = Sequence.(incr rx_isn) in
  (* TODO: improve this sanity check of tx_mss *)
  let tx_mss = match tx_mss with |None -> default_mss |Some mss -> min mss max_mss in
  let snd_una = tx_nxt in
  let rx_wnd = Int32.(shift_left (of_int rx_wnd) rx_wnd_scale) in
  let tx_wnd = Int32.(shift_left (of_int tx_wnd) tx_wnd_scale) in
  (* ssthresh is initialized per RFC 2581 to a large value so slow-start
     can be used all the way till first loss *)
  let ssthresh = tx_wnd in
  let cwnd = Int32.of_int (tx_mss * 2) in
  let fast_recovery = false in
  { snd_una; tx_nxt; tx_wnd; rx_nxt; rx_nxt_inseq; rx_nxt_lastack; rx_wnd; tx_wnd_scale; rx_wnd_scale;
    ssthresh; cwnd; tx_mss; fast_recovery }

(* Check if a sequence number is in the right range
   TODO: modulo 32 for wrap
 *)
let valid t seq =
  let redge = Sequence.(add t.rx_nxt (of_int32 t.rx_wnd)) in
  (* TODO: change temp fix to real max advertised window of the connection,
     deal with scaling etc *)
  let ledge = Sequence.(sub t.rx_nxt (Sequence.of_int 65535)) in 
  let r = Sequence.between seq ledge redge in
  (* printf "TCP_window: valid check for seq=%s for range %s[%lu] res=%b\n%!"
    (Sequence.to_string seq) (Sequence.to_string t.rx_nxt) t.rx_wnd r; *)
  r

(* Advance received packet sequence number *)
let rx_advance t b =
  t.rx_nxt <- Sequence.add t.rx_nxt (Sequence.of_int b)

(* Early advance received packet sequence number for packet ordering *)
let rx_advance_inseq t b =
  t.rx_nxt_inseq <- Sequence.add t.rx_nxt_inseq (Sequence.of_int b)

(* Next expected receive sequence number *)
let rx_nxt t = t.rx_nxt 
let rx_nxt_set_lastack t = t.rx_nxt_lastack <- t.rx_nxt; t.rx_nxt 
let rx_nxt_inseq t = t.rx_nxt_inseq
let rx_wnd t = t.rx_wnd

(* Check if there is pending data for which an ack has to be sent *)
let rx_pending_ack t = t.rx_nxt_lastack <> t.rx_nxt

(* TODO: scale the window down so we can advertise it correctly with
   window scaling on the wire *)
let set_rx_wnd t sz =
  t.rx_wnd <- sz

(* Take an unscaled value and scale it up *)
let set_tx_wnd t sz =
  let wnd = Int32.(shift_left (of_int sz) t.tx_wnd_scale) in
  t.tx_wnd <- wnd

(* transmit MSS of current connection *)
let tx_mss t =
  t.tx_mss

(* Advance transmitted packet sequence number *)
let tx_advance t b =
  t.tx_nxt <- Sequence.add t.tx_nxt (Sequence.of_int b)

(* An ACK was received - use it to adjust cwnd *)
let tx_ack t r =
  if t.fast_recovery then begin
    if Sequence.gt r t.snd_una then begin
      printf "EXITING fast recovery\n%!";
      t.snd_una <- r;
      t.cwnd <- t.ssthresh;
      t.fast_recovery <- false;
    end else begin
      t.cwnd <- (Int32.add t.cwnd (Int32.of_int t.tx_mss));
    end
  end else begin
    if Sequence.gt r t.snd_una then begin
      t.snd_una <- r;
    end;
    let cwnd_incr = match t.cwnd < t.ssthresh with
    | true -> Int32.of_int t.tx_mss
    | false -> max (Int32.div (Int32.of_int (t.tx_mss * t.tx_mss)) t.cwnd) 1l
    in
    t.cwnd <- Int32.add t.cwnd cwnd_incr
  end

let tx_nxt t = t.tx_nxt 
let tx_wnd t = t.tx_wnd
let tx_una t = t.snd_una
let tx_available t = 
  let inflight = Sequence.to_int32 (Sequence.sub t.tx_nxt t.snd_una) in
  let win = min t.cwnd t.tx_wnd in
  let avail_win = Int32.sub win inflight in
  let avail_win_norunts =
    match avail_win < Int32.of_int t.tx_mss with | true -> 0l | false -> avail_win in
  avail_win_norunts

let alert_fast_rexmit t seq =
  let inflight = Sequence.to_int32 (Sequence.sub t.tx_nxt t.snd_una) in
  let newssthresh = max (Int32.div inflight 2l) (Int32.of_int (t.tx_mss * 2)) in
  let newcwnd = Int32.add newssthresh (Int32.of_int (t.tx_mss * 2)) in
  printf "ENTERING fast recovery inflight=%d, ssthresh=%d -> %d, cwnd=%d -> %d\n%!"
    (Int32.to_int inflight)
    (Int32.to_int t.ssthresh)
    (Int32.to_int newssthresh)
    (Int32.to_int t.cwnd)
    (Int32.to_int newcwnd);
  t.fast_recovery <- true;
  t.ssthresh <- newssthresh;
  t.cwnd <- newcwnd
