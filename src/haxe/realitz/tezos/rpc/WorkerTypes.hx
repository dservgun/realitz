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

package realitz.tezos.rpc;
import haxe.Int64;
import haxe.ds.Option;
/*
(** The running status of an individual worker. *)
type worker_status =
  | Launching of Time.t
  | Running of Time.t
  | Closing of Time.t * Time.t
  | Closed of Time.t * Time.t * error list option


*/

enum WorkerStatus {
  Launching (timestamp : Date);
  Running (timestamp : Date);
  Closing (birth : Date, since : Date);
  Closed (birth : Date, since : Date);
  Crashed (birth : Date, since : Date, crasted : Array<Dynamic>);
}

/**
* Some invariants: need to verify this
* If a state is in closed, closing or crashed, 
* the 'dyn.birth' should be earlier than 'dyn.since';
* In simple terms a validator could not have crashed before its birth.
* Some invariants with reference to the previous state need to be captured here.
* For example a crashed validator cannot go back to running state. Need to 
* verify these states.
*/
class WorkerStatusEncoding {
  public static function fromJSON(dyn : Dynamic) : WorkerStatus {
    var workerStatus : Dynamic = dyn.status;
    trace('WorkerStatus $workerStatus');    
    switch(workerStatus.phase) {
      case "launching" : return Launching(workerStatus.timestamp);
      case "running" : return Running(workerStatus.since);
      case "closing" : 
        return Closing(workerStatus.birth, workerStatus.since);
      case "closed" : return Closed(workerStatus.birth, workerStatus.since);
      case "crashed" : 
        return Crashed (workerStatus.birth, 
            workerStatus.since, workerStatus.errors);
      case _ : throw 'Invalid phase $workerStatus';
    }
  }
}
/*
(** The runnning status of an individual request. *)
type request_status =
  { pushed : Time.t ;
    treated : Time.t ;
    completed : Time.t }
*/

enum RequestStatus {
  Pushed (timestamp : Int64);
  Treated (timestamp : Int64);
  Completed (timestamp : Int64);
}

class RequestStatusEncoding {
  public static function fromJSON(status : Dynamic) : RequestStatus {
    if (status.pushed != null) {
      return Pushed(status.pushed);
    }
    if (status.treated != null) {
      return Treated(status.treated);
    }
    if (status.completed != null) {
      return Completed(status.completed);
    }
    throw 'Invalid status $status';
  }
}

