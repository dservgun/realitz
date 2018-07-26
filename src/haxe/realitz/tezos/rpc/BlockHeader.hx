package realitz.tezos.rpc;

/*****************************************************************************/
/*                                                                           */
/* Open Source License                                                       */
/* Copyright (c) Dinkar Ganti, dinkar.ganti@gmail.com */
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

class BlockHeader {
  var level :  Int;
  var protoLevel : Int ;
  var predecessor : Bytes ;
  var timestamp : Bytes;
  var validationPass : Int; 
  var operationsHash : Array<Array<Bytes>>;
  var fitness : Array<Bytes>; 
  var context : Bytes;
}


class BlockHeaderWithData {
  var header : BlockHeader; 
  /* 
  * TODO: Bytes types wraps platform dependent bytes. 
  * Check on irc if this is the right way to doing this.
  */ 
  var data : Bytes;
}


class BlockResponse {
  var block : BlockHash; 
  var level : Int ;
  var errors : List<Error>;
}

class InjectBlock {
  var data : Bytes;
  var operations : 
    List <List < {branch : BlockHash, data : Bytes} > >;
}

class InjectOperation {
  var data : Bytes;
}

class OperationHash {
  var hash : String;
}

class Component {
  var name : String ;
  var _interface : Bytes;
  var implementation : Bytes;
}

class ProtocolHash { 
  var hash : String;
}

class ContextHash {
  var hash : String;
}
class InjectProtocol {
  var expectedEnvVersion : Int;
  var components : List<Component>;
} 

class Bootstrapped {
  var block : BlockHash ;
  var timestamp : Int64;
}

class Fitness {
  var fitness : List<Bytes>;
}

class BlockInformation {
  var chainId  : ChainId;
  var hash : BlockHash;
  var level : Int; 
  var proto : Int;
  var predecessor : BlockHash ;
  var timestamp : Int64;
  var validationPass : Int;
  var operationsHash : OperationHash;
  var fitness : Fitness;
  var context : ContextHash; 
  var protocolData : Bytes;
}

class IdPoint {
  var address : String;
  var port : Int;
}

class ConnectionVersion {
  var name : String;
  var major : Int;
  var minor : Int;
}

class Metadata {
  var data : Bytes; //TO Be completed
}
class ConnectionInformation {
  var incoming : Bool;
  var peerId : PeerId;
  var idPoint : IdPoint;
  var remoteSocketPort : Int;
  var versions : List<ConnectionVersion>;
  var privateConnection : Bool;
  var localMetadata : Metadata;
  var remoteMetadata : Metadata;
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
  * We rejected a connection after authentifying the remote peer.
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
  var timestamp : Int64;
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
  var totalSent : Int64;
  var totalReceived : Int64;
  var currentInflow : Int;
  var currentOutflow : Int;
}

class Monitor {
  var requestKind : PoolEvent;
  var timeStamp : Int64;
  var address : String;
  var port : Int;
}

class Peer {
  var score : Int;
  var trusted : Bool;
  var connectionMetadata : ConnectionMetadata;
  var peerState : PeerState;
  var reachableAt : IdPoint;
  var statistics : PeerStatistics;
  var lastFailedConnection : List<ConnectionInfo>;
  var lastRejectedConnection : List<ConnectionInfo>;
  var lastEstablishedConnection : List<ConnectionInfo>;
  var lastDisconnection : List<ConnectionInfo>;
  var lastSeen : List<ConnectionInfo>;
  var lastMiss : List<ConnectionInfo>;
}
