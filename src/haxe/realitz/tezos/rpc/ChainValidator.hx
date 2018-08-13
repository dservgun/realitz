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

import realitz.tezos.rpc.Types;
import realitz.tezos.rpc.WorkerTypes;

class ChainValidator {
  var chainId : ChainId;
  var status : WorkerStatus;
  public function new (chainValidator : Dynamic) {
    if (chainValidator != null) {
      //TODO: Check for null is needed because we are using
      //dynamic here. Maybe this should also be a static function.
      trace('Chain id ${chainValidator.chain_id}');
      trace('Chain status ${chainValidator.status}');      
      chainId = new ChainId(chainValidator.chain_id);
      status = WorkerStatusEncoding.fromJSON(chainValidator);
    }
  }

  public static function fromJSONArray(validators : Array<Dynamic>) {
    var result : List<ChainValidator> = new List();
    trace('Validators : $validators');
    for(validator in validators) {
      result.add(new ChainValidator(validator));
    }
    return result;
  }
}