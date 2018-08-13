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

import haxe.Int64;
import haxe.ds.Option;
import realitz.tezos.rpc.WorkerTypes;
import realitz.tezos.rpc.Types;

/*
* file : block_validator.mli
* val pending_requests : t -> (Time.t * Block_validator_worker_state.Request.view) list
val current_request : t -> (Time.t * Time.t * Block_validator_worker_state.Request.view) option
val last_events : t -> (Lwt_log_core.level * Block_validator_worker_state.Event.t list) list
  type view = {
    chain_id : Chain_id.t ;
    block : Block_hash.t ;
    peer: P2p_peer.Id.t option ;
  }

*/

class BlockRequestView {
  var chainId : ChainId;
  var block : BlockHash ;
  var peer : Option<PeerId>;
  public function new (dyn : Dynamic) {
    chainId = dyn.chain_id;
    block = dyn.block;
    if (dyn.peer == null){
      peer = None;
    }else {
      peer = Some(new PeerId(dyn.peer));
    }
  }
}

class BlockRequest {
  var timestamp : Date;
  var requestView : BlockRequestView;
  public function new(aDynamic : Dynamic) {
    trace('BlockRequest $aDynamic');
    if (aDynamic == null) {
      trace("Using defaults");
    }else {
      timestamp = aDynamic.timestamp;
      requestView = new BlockRequestView(aDynamic.request);      
    }
  }
  public static function fromArray(aDynArray : Array<Dynamic>) : List<BlockRequest> {
    trace('requests $aDynArray');
    var result : List<BlockRequest> = new List();
    var dynArrayLen = aDynArray.length;
    trace('Dynamic array length $dynArrayLen');
    if(dynArrayLen > 0) {
      for (element in aDynArray) {
        result.add(new BlockRequest(element));
      }      
    }else {
      trace("Empty array");
    }
    return result;
  }
}
class Backlog {
  var level : String;
  var events : List<WorkerStateEvent>;
  public function new (backlog : Dynamic) {
    level = backlog.level;
    events = WorkerStateEventEncoding.fromArray(backlog.events);
  }
  public static function fromArray (aDynArray : Array<Dynamic>) : List<Backlog> {
    var result : List<Backlog> = new List();
    for (element in aDynArray) {
      result.add(new Backlog(element));
    }
    return result;
  }
}

/*

module Event : sig
  type t =
    | Validation_success of Request.view * Worker_types.request_status
    | Validation_failure of Request.view * Worker_types.request_status * error list
    | Debug of string
  val level : t -> Logging.level
  val encoding : t Data_encoding.encoding
  val pp : Format.formatter -> t -> unit
end
*/

enum WorkerStateEvent {
    Debug (message : String);
    ValidationSuccess (view : BlockRequestView, status : RequestStatus);
    ValidationFailure (view : BlockRequestView, status : RequestStatus, errors : List<Error>);
}

class WorkerStateEventEncoding {
  public static function fromJSON (event : Dynamic) : WorkerStateEvent {
    if (event.message != null) {
      return Debug(event.message);
    }
    if (event.successful_validation != null) {
      var requestView = new BlockRequestView(event.successful_validation);
      var status : RequestStatus = RequestStatusEncoding.fromJSON(event.status);
      return (ValidationSuccess(requestView, status));
    }
    if (event.failed_validation != null) {
      var requestView = new BlockRequestView(event.failed_validation);
      var status : RequestStatus = RequestStatusEncoding.fromJSON(event.status);
      return (ValidationFailure(requestView, status, event.errors));
    }
    throw 'Invalid state event $event';
  }
  public static function fromArray(events : Array<Dynamic>) : List<WorkerStateEvent> {
    var result : List<WorkerStateEvent> = new List();
    for (event in events){
      result.add(fromJSON(event));
    }
    return result;
  }
}
class BlockValidator {
  var status (default, null) : WorkerStatus;
  var pendingRequests (default, null): List<BlockRequest>;
  var currentRequest (default, null) : BlockRequest;
  var backlog (default, null) : List<Backlog>;

  public function new (dyn : Dynamic) {
    trace('Block validator $dyn');
    status = WorkerStatusEncoding.fromJSON(dyn);
    pendingRequests = BlockRequest.fromArray(dyn.pending_requests);
    currentRequest = new BlockRequest(dyn.current_request);
    backlog = Backlog.fromArray(dyn.backlog);
  }
}