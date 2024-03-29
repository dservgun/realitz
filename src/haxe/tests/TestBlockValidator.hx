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

//TODO: add build file such that it picks up all the files.
import realitz.core.Property;
import realitz.tezos.rpc.BlockHeader;
import realitz.tezos.rpc.BlockHeaderMetadata;
import realitz.tezos.rpc.ChainValidator;
import realitz.tezos.rpc.BlockValidator;
import realitz.tezos.rpc.ContextConstants;
import realitz.tezos.rpc.Error;
import realitz.tezos.rpc.MetadataTypes;
import realitz.tezos.rpc.Michelson;
import realitz.tezos.rpc.MichelsonPrimitives;
import realitz.tezos.rpc.Operation;
import realitz.tezos.rpc.Types;
import realitz.tezos.rpc.WorkerTypes;
import realitz.tezos.rpc.encoding.Responses;
import realitz.tezos.rpc.encoding.Requests;
import realitz.tezos.rpc.encoding.ShellRequestAPI;
import realitz.tezos.rpc.encoding.RPCConfig;

//Integration test for block and chain validation.
class TestBlockValidator {
  static function testBlockValidator(config : RPCConfig) {
    var blockValidator = Shell.introspectBlockValidator(config);
    trace('Block validators $blockValidator');
  }
  static function testChainValidators(config : RPCConfig) {
    var chainValidators = Shell.introspectChainValidators(config);
    trace('Validators $chainValidators');
  }

  static function testChainPrevalidators(config : RPCConfig) {
    var prevalidators = Shell.introspectChainPrevalidators(config);
    trace('Prevalidators $prevalidators');
  }
  static function main () {
    var config = new RPCConfig("http://localhost", "18732");
    testBlockValidator(config);
    testChainValidators(config);
    testChainPrevalidators(config);
  }

}