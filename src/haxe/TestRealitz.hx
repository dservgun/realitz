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
import realitz.tezos.rpc.BlockPreValidator;
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

/**
* A simple test shell for testing all requests given a config. 
* Assuming localhost with a port.
*/
class TestRealitz {
  
  static function testGetChainId(config : RPCConfig, aChain : String) {
    trace(Shell.getChainId(config, aChain));
  }
  static function testBlocksForAChain(config : RPCConfig, aChain : String) {
    trace(Shell.getBlocksForAChain(config, aChain, None, None, None));
  }
  static function testInvalidBlocks(config : RPCConfig, aChain : String) {
    trace(Shell.getInvalidBlocks(config, aChain));
  }
  static function testGetMempool (config : RPCConfig, aChain : String) {
    trace(Shell.getMempool(config, aChain));
  }
  static function testMonitorBootstrapped(config : RPCConfig) {
    trace(Shell.monitorBootstrappedBlocks(config));
  }
  static function testMonitorHeadBlockForChain(config : RPCConfig) {
      trace("Monitor head block for a chain");
      trace(Shell.monitorHeadBlockForChain(config, "main", None));
    }
  static function testMonitorProtocols(config : RPCConfig) {
    trace("Monitor protocols");
    trace(Shell.monitorProtocols(config));
  }
  static function testMonitorValidBlocks(config : RPCConfig) {
    trace("Shell monitor valid blocks");
    trace(Shell.monitorValidBlocks(config, None, None, None));
  }
  static function testGetNetworkConnections(config : RPCConfig) {
    trace("Test get network connections");
    var peers : List<ConnectionInformation> = 
      Shell.getNetworkConnections(config);
    for (peer in peers) {
      trace(Shell.getPeerDetails(config, peer.peerId));
    }
    trace("Done querying.");
  }
  static function main () {
    var config = new RPCConfig("http://localhost", "18732");
    testGetChainId(config, "main");
    testBlocksForAChain(config, "main");
    testInvalidBlocks(config, "main");
    testGetMempool(config, "main");
    testMonitorBootstrapped(config);
    testMonitorHeadBlockForChain(config);
    testMonitorProtocols(config);
    //testMonitorValidBlocks(config);
    testGetNetworkConnections(config);
  }
}