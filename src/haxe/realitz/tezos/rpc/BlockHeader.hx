package realitz.tezos.rpc;

/*****************************************************************************/
/*                                                                           */
/* Open Source License                                                       */
/* Copyright (c) Dinkar Ganti, dinkar.ganti@gmail.com                        */
/*                                                                           */
/* Permission is hereby granted, free of charge, to any person obtaining a   */
/* copy of this software and associated documentation files (the "Software"),*/
/* to deal in the Software without restriction, including without limitation */
/* the rights to use, copy, modify, merge, publish, distribute, sublicense,  */
/* and/or sell copies of the Software, and to permit persons to whom the     */
/* Software is furnished to do so, subject to the following conditions:      */
/*                                                                           */
/* The above copyright notice and this permission notice shall be included   */
/* in all copies or substantial portions of the Software.                    */
/*                                                                           */
/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*/
/* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  */
/* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   */
/* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*/
/* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   */
/* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       */
/* DEALINGS IN THE SOFTWARE.                                                 */
/*                                                                           */
/*****************************************************************************/

import haxe.io.Bytes;
import haxe.Int64;
import haxe.ds.Option;
import realitz.tezos.rpc.Types;

/** 
Block header and the header with data refer to the 
types defined here : file : block_header.mli.

type shell_header = {
  level: Int32.t ;
  proto_level: int ; (* uint8 *)
  predecessor: Block_hash.t ;
  timestamp: Time.t ;
  validation_passes: int ; (* uint8 *)
  operations_hash: Operation_list_list_hash.t ;
  fitness: Fitness.t ;
  context: Context_hash.t ;
}

val shell_header_encoding: shell_header Data_encoding.t

type t = {
  shell: shell_header ;
  protocol_data: MBytes.t ;
}

*/



class BlockHeaderWithData {
  var header : BlockHeader; 
  /* 
  * TODO: Bytes types wraps platform dependent bytes. 
  * Check on irc if this is the right way to doing this.
  */ 
  var data : Bytes;
}



class Metadata {
  var data : Bytes; //TO Be completed
}

enum PoolEvent {
  TooFewConnections;
  TooManyConnections;
  NewPoint(point : IdPoint);
  NewPeer (peer : PeerId);
  /**
  * Garbage collection of known point table has been triggered. 
  */
  GCPoints;
  /**
  * Garbage collection of known peer ids table has been triggered.
  /*
  GCPeerIds;
  /**
  * We accepted a connection.
  */
  IncomingConnection(point : IdPoint);
  /**
  * We connected to a remote endpoint.
  */
  OutgoingConnection(point : IdPoint);
  /**
  * Authentication failed for the P2PPoint.
  */
  AuthenticationFailed(point : IdPoint);

  AcceptingRequest (point : IdPoint, identifier : PeerId, 
    peerId : PublicKey);

  /** 
  * We rejected a connection after authenticating the remote peer.
  */
  RejectingRequest (point : IdPoint, identifier : PeerId, 
    peerId : PublicKey);
  /**
  * The remote peer rejected our connection.
  */
  RequestRejected (point : IdPoint, identifier : PeerId, peerId : PeerId);

  /**
  * We successfully established an authenticated connection
  */
  ConnectionEstablished(identifier : ConnectionId, peer : PeerId);

  /**
  * A swap request received of a source.
  */
  SwapRequestReceived (source : PeerId);
  /**
  * A swap ack has been received
  */
  SwapAckReceived (source : PeerId);

  /**
  * A swap request has been sent out.
  */
  SwapRequestSent (source : PeerId);
  /**
  * Swap ack has been sent
  */
  SwapAckSent (source : PeerId);

  /**
  * A swap request has been ignored.
  */
  SwapRequestIgnored(source : PeerId);
  /**
  * Swap operation has succeeded.
  */
  SwapSuccess (source : PeerId);
  /**
  * Swap operation has failed.
  */
  SwapFailure (source : PeerId);
  /**
  * We decided to close the connection.
  */
  Disconnection (peerId : PeerId);
  /**
  * The connection was closed for external reason.
  */
  ExternalDisconnection (peerId : PeerId);
}

/*
file : p2p_connection.mli
    | Swap_ack_sent of { source : P2p_peer_id.t }
    (** A swap ack has been sent *)
    | Swap_request_ignored of { source : P2p_peer_id.t }
    (** A swap request has been ignored *)
    | Swap_success of { source : P2p_peer_id.t }
    (** A swap operation has succeeded *)
    | Swap_failure of { source : P2p_peer_id.t }
    (** A swap operation has failed *)

    | Disconnection of P2p_peer_id.t
    (** We decided to close the connection. *)
    | External_disconnection of P2p_peer_id.t
    (** The connection was closed for external reason. *)

*/

class ConnectionInfo {
  var connectionId : IdPoint;
  var timestamp : String;
  
  public function new (dyn : Array<Dynamic>) {
    trace('ConnectionInfo $dyn');
    connectionId = new IdPoint(dyn[0]);
    timestamp = dyn[1];
  }
  public static function parseFromList(infoList : Array<Dynamic>) : List<ConnectionInfo> {
    var result : List<ConnectionInfo> = new List();
    if (infoList == null) {
      return result;
    }
    trace('Parse from list $infoList');
    result.add(new ConnectionInfo(infoList));
    return result;
  }
}

class ConnectionMetadata {
  var disableMempool : Bool;
  var privateNode : Bool;
}
enum PeerState {
  Running;
  Accepted;
  Disconnected;
}

class PeerStatistics {
  var totalSent (default, null) : Int64;
  var totalReceived (default, null) : Int64;
  var currentInflow (default, null) : Int;
  var currentOutflow (default, null) : Int;
  public function new(dyn : Dynamic) {
    trace('New : $dyn');
    totalSent = dyn.total_sent;
    totalReceived = dyn.total_received;
    currentInflow = dyn.current_inflow;
    currentOutflow = dyn.current_outflow;
  }
  //TODO: How do i embed functions inside an enum.
  public static function parsePeerStateJSON(aDyn : String) : PeerState {
    trace("Parse peer state " + aDyn);
    switch(aDyn){
      case "running" : return Running;
      case "accepted" : return Accepted;
      case "disconnected" : return Disconnected;
      case _ : throw ('Invalid peer state $aDyn');
    }
  }

  public static  function parseJSON(dyn : Dynamic) : PeerStatistics {
    return (new PeerStatistics(dyn));
  }
}

enum RequestKind {
  RejectingRequest;
  IncomingRequest;
  Disconnection;
  ExternalDisconnection;
  ConnectionEstablished;
  RequestRejected;
}

class Monitor {
  var requestKind (default, null): RequestKind;
  var timeStamp (default, null) : String;
  var address (default, null) : String;
  var port (default, null) : String;

  public function parseRequestKindEventJSON(kindDyn : String) : RequestKind{
    switch(kindDyn){
      case "rejecting_request" : return RejectingRequest;
      case "incoming_request" : return IncomingRequest;
      case "disconnection" : return Disconnection;
      case "external_disconnection" : return ExternalDisconnection;
      case "connection_established" : return ConnectionEstablished;
      case "request_rejected" : return RequestRejected;
      case _ : throw 'Invalid event $kindDyn';
    }
  }
  public function new(aDynamic : Dynamic) {
    requestKind = parseRequestKindEventJSON(aDynamic.kind);
    timeStamp = aDynamic.timestamp;
    address = aDynamic.address;
    port = aDynamic.port;
  }

  public static function parseFromJSONList(monitorEvents : Array<Dynamic>) : List<Monitor>{
    var result : List<Monitor> = new List();
    for (event in monitorEvents) {
      result.add(new Monitor(event));
    }
    return result;
  }

}


class Peer {
  function new(dyn : Dynamic) {
    score = dyn.score;
    trusted = dyn.trusted;
    peerState = PeerStatistics.parsePeerStateJSON(dyn.state);
    var rAt = dyn.reachable_at;
    trace('Reachable $rAt');
    reachableAt = IdPoint.parseJSON(rAt);
    statistics = PeerStatistics.parseJSON(dyn.stat);
    lastFailedConnection = ConnectionInfo.parseFromList(dyn.last_failed_connection);
    lastRejectedConnection = ConnectionInfo.parseFromList(dyn.last_rejected_connection);
    lastEstablishedConnection = ConnectionInfo.parseFromList(dyn.last_established_connection);
    lastDisconnection = ConnectionInfo.parseFromList(dyn.last_disconnection);
    lastSeen = ConnectionInfo.parseFromList(dyn.last_seen);
    lastMiss = ConnectionInfo.parseFromList(dyn.last_miss);    

  }
  var score (default, null) : Int;
  var trusted (default, null) : Bool;
  var peerState (default, null) : PeerState;
  var reachableAt (default, null) : IdPoint;
  var statistics (default, null) : PeerStatistics;
  var lastFailedConnection (default, null) : List<ConnectionInfo>;
  var lastRejectedConnection (default, null) : List<ConnectionInfo>;
  var lastEstablishedConnection(default, null) : List<ConnectionInfo>;
  var lastDisconnection (default, null) : List<ConnectionInfo>;
  var lastSeen (default, null) : List<ConnectionInfo>;
  var lastMiss (default, null) : List<ConnectionInfo>;

  public static function parseJSON(dyn : Dynamic) : Peer {
    return (new Peer(dyn));
  }
}


