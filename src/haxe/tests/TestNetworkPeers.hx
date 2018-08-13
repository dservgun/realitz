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
* Test network peers.
* Assuming localhost with a port.
*/
class TestNetworkPeers {
  static function testGetNetworkPeers(config : RPCConfig) {
    var peers : List<PeerPair>= Shell.getNetworkPeers(config);
    for (peerPair in peers) {
      trace(Shell.getNetworkPeer(config, peerPair.peerId));
      var banStatus = Shell.checkPeerBanStatus(config, peerPair);
      trace(banStatus);
      var monitorLog = Shell.monitorPeerLog(config, peerPair, None);
      trace(monitorLog);
    }
  }
  static function testNetworkPoints(config : RPCConfig) {
    var points : List<PointPair> = Shell.getNetworkPoints(config, None);
    trace(points);
  }
  //TODO : How to pass an ipv6 address?
  static function testNetworkPoint(config : RPCConfig) {
    var peers : List<PeerPair> = 
      Shell.getNetworkPeers(config);
    for (peerP in peers) {
      var networkPoint = 
        Shell.getNetworkPoint(config, peerP.peer.ipAddress());
      trace(networkPoint);
    }
  }
  static function testNetworkStat(config : RPCConfig) {
    trace(Shell.networkStat(config));
  }  
  static function testNetworkVersions(config : RPCConfig) {
    trace(Shell.getNetworkVersions(config));
  }
  static function testGetProtocols(config : RPCConfig) {
    var protocols : List<String> = Shell.getProtocols(config);
    for(protocol in protocols) {
      trace(Shell.getProtocol(config, new ProtocolHash(protocol)));
    }
    trace(Shell.getProtocols(config));
  }
  static function main () {
    var config = new RPCConfig("http://localhost", "18732");
    testGetNetworkPeers(config);
    testNetworkPoints(config);
    testNetworkStat(config);
    testNetworkVersions(config);
    testGetProtocols(config);
    //testNetworkPoint(config);
  }
}