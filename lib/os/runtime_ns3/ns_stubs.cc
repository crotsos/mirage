/*
 * Copyright (c) 2012 Charalampos Rotsos <cr409@cl.cam.ac.uk>
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
 */

#include <iostream>
#include <fstream>

#include <ns3/core-module.h>
#include <ns3/network-module.h>
#include <ns3/csma-module.h>
#include <ns3/internet-module.h>
#include <ns3/applications-module.h>
#include <ns3/log.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>

#include <hash_map>

using namespace std;
using namespace ns3;

NS_LOG_COMPONENT_DEFINE ("MirageExample");

ns3::Time timeout = ns3::Seconds (0);

#ifdef  __cplusplus
extern "C" {
#endif

CAMLprim value ocaml_ns3_run(value v_null);
CAMLprim value ocaml_ns3_add_node(value ocaml_name);
CAMLprim value ocaml_ns3_add_timer_event(value p_ts, value p_id);
#include <caml/callback.h>

#ifdef  __cplusplus
}
#endif

/*
 * Ns3 event handler functions
 */

static void
DeviceHandler(Ptr<NetDevice>) {
  printf("New device registered on node\n");
}

static void
TimerEventHandler(int id) {
  caml_callback(*caml_named_value("timer_wakeup"), Val_int((int)id));
}

CAMLprim value 
ocaml_ns3_add_timer_event(value p_ts, value p_id) {
  double ts = Double_val(p_ts);
  int id = Int_val(p_id);

  Simulator::Schedule(Seconds (ts), &TimerEventHandler, id );
  return Val_unit;
}

/*
 * Node and link manipulation function
 */

map<string, Ptr<Node> > nodes;

CAMLprim value
ocaml_ns3_add_node(value ocaml_name)
{
  string name =  string(String_val(ocaml_name));
  fprintf(stderr, "Adding node %s\n", name.c_str());

  // create a single node for the new host
  NodeContainer node;
  node.Create(1);
  // add in the last hashmap
  nodes[name] = node.Get(0);

  // register handlers in case a new network device is added
  // on the node
  nodes[name]->RegisterDeviceAdditionListener(MakeCallback(&DeviceHandler));

  return ocaml_name;
}

static void
InitNodeState(void) {
  caml_callback(*caml_named_value("init"), Val_unit);
}

/*
 * Main run mechanism
 */

CAMLprim value
ocaml_ns3_run(value v_null) {
  Simulator::Schedule(Seconds (0.0), &InitNodeState );
  
  Simulator::Run ();
  Simulator::Destroy ();
  return Val_int(0);
}
