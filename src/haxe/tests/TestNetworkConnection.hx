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
class TestNetworkConnection {
  static function testGetNetworkConnections(config : RPCConfig) {
    trace("Test get network connections");
    var peers : List<ConnectionInformation> = 
      Shell.getNetworkConnections(config);
    for (peer in peers) {
      trace(peer);
      trace(Shell.getPeerDetails(config, peer.peerId));
    }
  }

  static function testGetNetworkLog(config : RPCConfig) {
    var logEvents = Shell.getNetworkLog(config);
    trace(logEvents);
  }  
  static function testGetNetworkPeers(config : RPCConfig) {
    var peers = Shell.getNetworkPeers(config);
    for (peer in peers) {
      trace(Shell.getNetworkPeer(config, peer.peerId));
    }
    trace(peers);
  }
  static function main () {
    var config = new RPCConfig("http://localhost", "18732");
    testGetNetworkConnections(config);
    testGetNetworkLog(config);
    testGetNetworkPeers(config);
  }
}