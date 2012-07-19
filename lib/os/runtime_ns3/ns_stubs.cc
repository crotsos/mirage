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

#include <arpa/inet.h>

#include <ns3/core-module.h>
#include <ns3/network-module.h>
#include <ns3/csma-module.h>
#include <ns3/internet-module.h>
#include <ns3/applications-module.h>
#include <ns3/log.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>

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
CAMLprim value ocaml_ns3_add_link(value ocaml_node_a, value ocaml_node_b);
CAMLprim value ocaml_ns3_add_timer_event(value p_ts, value p_id);
CAMLprim value caml_pkt_write(value v_node_name, value v_id, value v_ba, 
    value v_off, value v_len);
CAMLprim value caml_queue_check(value v_name,  value v_id);
CAMLprim value
caml_register_check_queue(value v_name,  value v_id);

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/bigarray.h>
#include <caml/memory.h>

#ifdef  __cplusplus
}
#endif

/*
 * Ns3 event handler functions
 */

map<string, Ptr<Node> > nodes;

void 
hexdump(uint8_t *buf, int len) {
  int p = 0;
  int count = 0;

  while(p < len) {
    printf("%02x", buf[p]);
    if(count == 7)
      printf(" ");
    else if(count == 15)
      printf("\n");
    p++;
    if(count == 15)
      count = 0;
    else
      count++;
  }
  printf("\n");
}

static string
getHostName(Ptr<NetDevice> dev) {
  // find node name 
  map<string, Ptr<Node> >::iterator it;
  for (it = nodes.begin(); it != nodes.end(); it++) {
    if (dev->GetNode()->GetId() == it->second->GetId()) {
      return it->first;
    }
  }

  return string("");
}

/*
 * Timed event methods
 */
int event_counter;
static void
TimerEventHandler(int id) {
  caml_callback(*caml_named_value("timer_wakeup"), Val_int((int)id));
}

CAMLprim value 
ocaml_ns3_add_timer_event(value p_ts, value p_id) {
  CAMLparam2(p_ts, p_id);
  double ts = Double_val(p_ts);
  int id = Int_val(p_id);

  EventId ev = Simulator::Schedule(Seconds (ts), &TimerEventHandler, id );
  CAMLreturn( Val_unit );
}

/*
 * Network related functions
 */
static void
DeviceHandler(Ptr<NetDevice> dev) {
//  printf("New device registered on node\n");
  string node_name;
  uint8_t *mac;

  node_name = getHostName(dev);

  // fetch device mac address
  mac = (uint8_t *)malloc(Address::MAX_SIZE);
  bzero(mac, Address::MAX_SIZE);
  dev->GetAddress().CopyTo(mac);
  int mac_len = dev->GetAddress().GetLength();
  CAMLlocal1( ml_mac );
  ml_mac = caml_alloc_string(mac_len);
  memcpy( String_val(ml_mac), mac, mac_len );
  free(mac);

  // passing event to caml code
  caml_callback3(*caml_named_value("plug_dev"), 
      caml_copy_string((const char *)node_name.c_str()),
      Val_int(dev->GetIfIndex()), ml_mac);
}

bool
PktDemux(Ptr<NetDevice> dev, Ptr<const Packet> pkt, uint16_t proto, 
    const Address &src, const Address &dst, NetDevice::PacketType type) {
//  printf("packet demux...\n");
 
  value ml_data;
  caml_register_global_root(&ml_data);
  int pkt_len = pkt->GetSize();
  ml_data = caml_alloc_string(pkt_len);
  pkt->CopyData((uint8_t *)String_val(ml_data), pkt_len);
  
  // find host name
  string node_name = getHostName(dev);

  //printf("node %s.%d packet\n", node_name.c_str(), dev->GetIfIndex());
  // call packet handling code in caml
  caml_callback3(*caml_named_value("demux_pkt"), 
      caml_copy_string((const char *)node_name.c_str()),
      Val_int(dev->GetIfIndex()), ml_data );
  caml_remove_global_root(&ml_data);
//  printf("packet demux end...\n");
  return true;
}

CAMLprim value
caml_pkt_write(value v_node_name, value v_id, value v_ba, 
    value v_off, value v_len) {
  CAMLparam5(v_node_name, v_id, v_ba, v_off, v_len);
  
  uint32_t ifIx = (uint32_t)Int_val(v_id);
  string node_name = string(String_val(v_node_name));
  int len = Int_val(v_len), off = Int_val(v_off);

  //get a pointer to the packet byte data
  uint8_t *buf = (uint8_t *) Caml_ba_data_val(v_ba);
  Ptr< Packet> pkt = Create<Packet>(buff + off, len );

  // rther proto of the packet. 
  uint16_t proto = ntohs(*(uint16_t *)(buf + off + 12));

  // find the right device for the node and send packet
  Ptr<Node> node = nodes[node_name];

  Mac48Address mac_dst;
  mac_dst.CopyFrom(buf + off);
  for (uint32_t i = 0; i < node->GetNDevices (); i++) 
    if(node->GetDevice(i)->GetIfIndex() == ifIx) { 
      if(!node->GetDevice(i)->Send(pkt, mac_dst, proto))
        fprintf(stdout, "%f: packet dropped...\n",
            (long)Simulator::Now().GetMicroSeconds() / 1e6);
    }
  CAMLreturn( Val_unit ); 
}

bool
check_queue_size(string name, int ifIx) {
  /* TODO: not sure how volatile is the default queue len */
  const uint32_t queue_len = 100;
  Ptr<DropTailQueue> q = 
    nodes[name]->GetDevice(ifIx)->GetObject<CsmaNetDevice>()->GetQueue()->GetObject<DropTailQueue>();
  return (queue_len > q->GetNPackets());
}

/*  true -> queue is not full, false queue is full */
CAMLprim value
caml_queue_check(value v_name,  value v_id) {
  CAMLparam2(v_name, v_id);
  string name =  string(String_val(v_name));
  int ifIx = Int_val(v_id);
  if(check_queue_size(name, ifIx) )
    CAMLreturn(Val_true);
  else 
    CAMLreturn(Val_false);
}

static void
NetQueueCheckHandler(string name, int ifIx) {
  if(check_queue_size(name,ifIx)) {
    caml_callback2(*caml_named_value("unblock_device"),
        caml_copy_string((const char *)name.c_str()),
        Val_int(ifIx));
  } else {
    Time ts = MicroSeconds(Simulator::Now().GetMicroSeconds() + 1);
    Simulator::Schedule(ts, &NetQueueCheckHandler, name, ifIx);
  }
}

CAMLprim value
caml_register_check_queue(value v_name,  value v_id) {
  CAMLparam2(v_name, v_id);
  string name =  string(String_val(v_name));
  int ifIx = Int_val(v_id);
  Time ts = MicroSeconds(Simulator::Now().GetMicroSeconds() + 1);
  Simulator::Schedule(ts, &NetQueueCheckHandler, name, ifIx);
  CAMLreturn(Val_unit);
}

/*
 * Node and link manipulation function
 */
CAMLprim value
ocaml_ns3_add_node(value ocaml_name)
{
  CAMLparam1( ocaml_name );
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

  CAMLreturn( ocaml_name );
}

CAMLprim value
ocaml_ns3_add_link(value ocaml_node_a, value ocaml_node_b) {
  CAMLparam2(ocaml_node_a, ocaml_node_b);
  string node_a = string(String_val(ocaml_node_a));
  string node_b = string(String_val(ocaml_node_b));
  fprintf(stderr, "Adding link between nodes %s - %s\n", 
      node_a.c_str(), node_b.c_str());

  // create a single node for the new host
  NodeContainer cont = NodeContainer(nodes[node_a], nodes[node_b]);
  CsmaHelper csma;
  csma.SetChannelAttribute("Delay", TimeValue (MilliSeconds (1)));
  
  Ptr<DropTailQueue> q = Create<DropTailQueue>();
  csma.SetDeviceAttribute("TxQueue", PointerValue(q));

  NetDeviceContainer link = csma.Install(cont);

  //setup packet handler
  link.Get(0)->SetPromiscReceiveCallback(MakeCallback(&PktDemux));
  link.Get(1)->SetPromiscReceiveCallback(MakeCallback(&PktDemux));


  //capture pcap trace
  string filename = string("openflow-switch-")+node_a+string("-")+node_b;
  csma.EnablePcapAll (filename, false);

  CAMLreturn ( Val_unit );
}

/*
 * A method to call the run functions of the hosts on time 0 of
 * the simulation.
 */
static void
InitNodeState(void) {
  caml_callback(*caml_named_value("init"), Val_unit);
}

/*
 * Main run mechanism
 */

CAMLprim value
ocaml_ns3_run(value v_null) {
  CAMLparam1(v_null);
  Simulator::Schedule(Seconds (0.0), &InitNodeState );
  
  Simulator::Run ();
  Simulator::Destroy ();
  CAMLreturn ( Val_unit );
}
