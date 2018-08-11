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
import realitz.tezos.rpc.ConnectionInfo;

enum PeerState {
  Requested;
  Running;
  Accepted;
  Disconnected;
}

class PeerStateUtil {
  //TODO: How do i embed functions inside an enum.
  public static function parsePeerState(aDyn : String) : PeerState {
    trace("Parse peer state " + aDyn);
    switch(aDyn){
      case "running" : return Running;
      case "accepted" : return Accepted;
      case "disconnected" : return Disconnected;
      case _ : throw ('Invalid peer state $aDyn');
    }
  }
}

class PointState {
  var point : Option<PeerId>;
  var peerState : PeerState;
  function new (p : Option<PeerId>, pState : PeerState) {
    point = p;
    peerState = pState;
  }
  public static function parseJSON(dyn : Dynamic) : PointState {
    var eventKind = dyn.event_kind;
    switch(eventKind) {
      case "requested" : return (new PointState(None, Requested));
      case "accepted" : return(new PointState(Some(dyn.p2p_peer_id), Accepted));
      case "running" : return(new PointState(Some(dyn.p2p_peer_id), Running));
      case "disconnected" : return (new PointState(None, Disconnected));
      case _ : throw ('Invalid event kind $eventKind.');
    }
  }
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

  public static  function parseJSON(dyn : Dynamic) : PeerStatistics {
    return (new PeerStatistics(dyn));
  }
}


/**
* The Peer information. Using 'dyn' to create an instance from 
* a dynamic type.
*/
class Peer {
  function new(dyn : Dynamic) {
    score = dyn.score;
    trusted = dyn.trusted;
    peerState = PeerStateUtil.parsePeerState(dyn.state);
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


class Point {

  function new(dyn : Dynamic) {
    score = dyn.score;
    trusted = dyn.trusted;
    pointState = PointState.parseJSON(dyn.state);
    p2pPeerId = dyn.p2p_peer_id;
    lastFailedConnection = dyn.last_failed_connection;
    lastRejectedConnection = ConnectionInfo.parseFromList(dyn.last_rejected_connection);
    lastEstablishedConnection = ConnectionInfo.parseFromList(dyn.last_established_connection);
    lastDisconnection = ConnectionInfo.parseFromList(dyn.last_disconnection);
    lastSeen = ConnectionInfo.parseFromList(dyn.last_seen);
    lastMiss = dyn.last_miss;
  }
  var score (default, null) : Int;
  var trusted (default, null) : Bool;
  var greyListedUntil (default, null) : Date;
  var pointState (default, null) : PointState;
  var p2pPeerId (default, null) : PeerId;
  var lastFailedConnection (default, null) : Date;
  var lastRejectedConnection (default, null) : List<ConnectionInfo>;
  var lastEstablishedConnection(default, null) : List<ConnectionInfo>;
  var lastDisconnection (default, null) : List<ConnectionInfo>;
  var lastSeen (default, null) : List<ConnectionInfo>;
  var lastMiss (default, null) : List<ConnectionInfo>;

  public static function parseJSON(dyn : Dynamic) : Point {
    return (new Point(dyn));
  }
}
