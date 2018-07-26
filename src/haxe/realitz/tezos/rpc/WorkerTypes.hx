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
  Launching (timestamp : Int64);
  Running (timestamp : Int64);
  Closing (start : Int64, end : Int64);
  Closed (start : Int64, end : Int64, errors : Option<List<Error>>);
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
