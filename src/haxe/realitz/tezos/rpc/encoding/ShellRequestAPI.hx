package realitz.tezos.rpc.encoding;

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

import realitz.tezos.rpc.Types;
import realitz.tezos.rpc.BlockHeader;
import haxe.io.Bytes;
import haxe.Http;
import haxe.ds.Option;
import realitz.tezos.rpc.encoding.Requests.InjectBlock;
import realitz.tezos.rpc.encoding.Requests.InjectOperation;
import realitz.tezos.rpc.encoding.Requests.InjectProtocol;
import realitz.tezos.rpc.encoding.Responses.InvalidBlockResponse;
import realitz.tezos.rpc.encoding.Responses.Bootstrapped;
import realitz.tezos.rpc.encoding.Responses.Mempool;
import realitz.tezos.rpc.encoding.RPCConfig;
import realitz.tezos.rpc.Peer;
import realitz.tezos.rpc.ContextConstants;

typedef StateFilter = String;
class Shell {

  private static function setOptionalParameters<A>
    (aParameter : Option<A>, paramName : String, request : Http): Http {
      switch (aParameter) {
        case (Some(aParam)) : return (request.setParameter(paramName, "$aParam"));
        case None : return request;
      }
    }
  private static function setLength (aLength : Option<Int>, request : Http) : Http {
    return (setOptionalParameters(aLength, "length", request));
  }
  private static function setHead (aHead : Option<BlockHash>, request : Http) : Http {
    return (setOptionalParameters(aHead, "head", request));
  }
  private static function setMinDate(aDate : Option<Date>, request : Http) : Http {
    return (setOptionalParameters(aDate, "min_date", request));
  }

  private static function setNextProtocol(aProtocolHash : Option<ProtocolHash>, request : Http) : Http {
    switch(aProtocolHash) {
      case (Some(aProtoHash)) : 
        return (setOptionalParameters(Some(aProtoHash.hash), "next_protocol", request));
      case None : return request;
    }
  }
  private static function setProtocol(aProtocolHash : Option<ProtocolHash>, request : Http) : Http {
    switch(aProtocolHash) {
      case (Some(aProtoHash)) : 
        return (setOptionalParameters(Some(aProtoHash.hash), "protocol", request));
      case None : return request;
    }
  }

  private static function setChainIdParameter(aChainId : Option<ChainId>, request : Http) : Http {
    switch (aChainId) {
      case (Some(aCh)) : 
        return (setOptionalParameters(Some(aCh.hash), "chain_id", request));
      case None : return request;
    }
  }
  public static function getBlocksForAChain(config : RPCConfig, chainId : String, 
    length : Option<Int>, 
    head  : Option<BlockHash>, 
    minDate : Option<Date>) : List<List<BlockHash>> {
    var requestString = "chains" + "/" + chainId + "/" + "blocks";
    trace(requestString);
    var httpRequest : Http =  config.getHttpWithPath(requestString);
    var httpRequest1 : Http = setLength(length, httpRequest);
    var httpRequest2 : Http = setHead(head, httpRequest1);
    var httpRequest3 : Http = setMinDate(minDate, httpRequest2);
    httpRequest3.request();
    var res : Null<String> = httpRequest3.responseData;
    trace("Result " + res);
    var result : List<List<BlockHash>> = new List();
    if (res != null) {
      var resArray : Array<Array<String>> = haxe.Json.parse(res);
      for (router in resArray){
        var outList = new List();
        for(rinner in router) {
          outList.add(new BlockHash(rinner));
        }
        result.add(outList);
      }
      return result;
    }else {
      return (new List());
    }
  }

  public static function getChainId (config : RPCConfig, chainId : String) : Option<ChainId> {
    var httpRequest : Http = config.getHttpWithPath("chains/" + chainId + "/chain_id");
    trace(httpRequest);
    httpRequest.setHeader("Content-type", "application/json");
    httpRequest.request();
    return (Some(new ChainId(haxe.Json.parse(httpRequest.responseData))));
  }

  public static function getInvalidBlocks(config : RPCConfig, chainId : String) : List<InvalidBlockResponse> {
    var httpRequest : Http = config.getHttpWithPath('chains/$chainId/invalid_blocks');
    httpRequest.request();
    var res : Null<String> = httpRequest.responseData;
    if (res != null) {
      trace(res);
      return (haxe.Json.parse(res));
    }else {
      return (new List());
    }
  }

  static function getInvalidBlock(config : RPCConfig, chainId : String, blockHash : String) : Option<InvalidBlockResponse> {
    var httpRequest : Http = config.getHttpWithPath(
      'chains/$chainId/invalid_blocks/$blockHash'
    );
    httpRequest.request();
    var res : Null<String> = httpRequest.responseData;
    if (res != null) {
      return (Some(haxe.Json.parse(res)));
    }else {
      return None;
    }
  }

  //TODO :
  static function deleteInvalidBlock(config : RPCConfig, chainId : String, blockHash : String) {
    var httpRequest : Http = config.getHttpWithPath(
      "chains/$chainId/invalid_blocks/$blockHash"
    );
    throw "Function not supported.";
  }

  public static function getMempool (config : RPCConfig, chainId : String) : Option<Mempool> {
    var httpRequest : Http = config.getHttpWithPath(
      'chains/$chainId/mempool'
    );
    httpRequest.request();
    var res : Null<String> = httpRequest.responseData;
    if (res != null) {
      var result : Dynamic = haxe.Json.parse(res);
      trace(result);
      return Some(Mempool.fromDynamic(result));
    }else {
      return None;
    }
  } 
  /**
    Injection operations
  */ 
  static function injectBlock (config : RPCConfig, chainId : String, aBlock : InjectBlock) {
    
    var httpRequest : Http = config.getHttpWithPath(
      "injection/block"
    );
    httpRequest.setHeader("Content-type", "application/json");
    httpRequest.setPostData(InjectBlock.toDynamic(aBlock));
    httpRequest.request(true);
    var res : Null<String> = httpRequest.responseData;
    if (res != null) {
      var result : Dynamic = haxe.Json.parse(res);
      trace("Result " + res);
    }else {
      trace("No data found");
    }
  }
  static function injectOperation(config : RPCConfig, operation : InjectOperation) {
    var httpRequest : Http = config.getHttpWithPath("injection/operation");
    httpRequest.setHeader("Content-type", "application/json");
    httpRequest.setPostData(operation.data);
    httpRequest.request(true);
    var res : Null<String> = httpRequest.responseData;
    if (res != null) {
      trace ("Result " + res);
    }else {
      trace ("Inject operation for $operation, failed");
    }
  }
  static function injectProtocol(config : RPCConfig, protocol : InjectProtocol) {
    var httpRequest : Http = config.getHttpWithPath("injection/protocol");
    httpRequest.setHeader("Content-type", "application/json");
    httpRequest.setPostData(protocol.toJSON());
    httpRequest.request(true);
    var res : Null<String> = httpRequest.responseData;
    if (res != null) {
      trace("Result " + res);
    }else {
      trace("Inject protocol for $protocol failed");
    }
  }
  /**
  * Monitoring operations.
  */
  public static function monitorBootstrappedBlocks(config : RPCConfig) : Option<Bootstrapped> {
    var httpRequest : Http = config.getHttpWithPath("monitor/bootstrapped");
    httpRequest.request();
    var res : Null<String> = httpRequest.responseData;
    if (res != null) {
      var result = Bootstrapped.fromJSON(res);
      return (Some (result));
    }else {
      return None;
    }
  }
  public static function monitorHeadBlockForChain (config : RPCConfig, chainId : String, nextProtocol : Option<ProtocolHash>) : Option<BlockHash> {
    var httpRequestO : Http = config.getHttpWithPath('monitor/heads/$chainId');
    var httpRequest1 : Http = setNextProtocol(nextProtocol, httpRequestO);
    httpRequest1.request();
    var res : Null<String> = httpRequest1.responseData;
    if (res != null) {
      return (Some (new BlockHash(res)));
    }else {
      return None;
    }
  }

  //TODO : FIX ME (the documentation from the main source).
  public static function monitorProtocols (config : RPCConfig) : Option<BlockHash> {
    var httpRequest : Http = config.getHttpWithPath("monitor/protocols");
    httpRequest.request();
    var res : Null<String> = httpRequest.responseData;
    if (res != null) {
      return (Some (new BlockHash(res)));
    }else {
      return None;
    }
  }

  public static function monitorValidBlocks(config : RPCConfig, protocol : Option<ProtocolHash>, nextProtocol : Option<ProtocolHash>, 
  chainId : Option<ChainId>) : Option<ValidBlockSummary> {
    var httpRequest : Http = config.getHttpWithPath("monitor/valid_blocks");
    var httpRequest0 : Http = setProtocol(protocol, httpRequest);
    var httpRequest1 : Http = setNextProtocol(protocol, httpRequest0);
    var httpRequest2 : Http = setChainIdParameter(chainId, httpRequest1);
    trace(httpRequest + " " + httpRequest2);
    httpRequest2.request();
    var res : Null<String> = httpRequest2.responseData;
    if (res != null){
      trace("Result:" + res);
      var dyn : Dynamic = haxe.Json.parse(res);
      return Some(new ValidBlockSummary(dyn));
    }else {
      return None;
    }
  }

  /** Network operations */
  public static function getNetworkConnections(config : RPCConfig) : 
    List<ConnectionInformation> {
      var httpRequest : Http = config.getHttpWithPath("network/connections");
      httpRequest.request();
      var res : Null<String> = httpRequest.responseData;
      if (res != null) {
        var dyn : Array<Dynamic> = haxe.Json.parse(res);
        return (ConnectionInformation.parseJSON(dyn));
      }else {
        return (new List());
      }
    }

  public static function getPeerDetails(config : RPCConfig, 
    peerId : PeerId) : Option<ConnectionInformation> {
      var httpRequest : Http = 
        config.getHttpWithPath('network/connections/$peerId');
      httpRequest.request();
      var res : Null<String> = httpRequest.responseData;
      if (res != null) {
        var dyn : Dynamic = haxe.Json.parse(res);
        trace('GET peer details : $dyn');
        return (Some (new ConnectionInformation(dyn)));
      }else {
        return None;
      }
    }

  public static function clearGreyList(config : RPCConfig) {
    var httpRequest : Http = 
      config.getHttpWithPath('network/greylist/clear');
    httpRequest.request();
  }

  public static function getNetworkLog(config : RPCConfig): Null<String> {
    var httpRequest : Http = 
      config.getHttpWithPath('network/log');
    httpRequest.request();
    var res : Null<String> = httpRequest.responseData;
    trace(res);
    return res;
  }

  //TODO: Create a task that sorts peers by load and 
  //that are not banned, grey listed at the time. 
  //Queue peers somewhere where the peer is nearing the term 
  //limit of being greylisted. 
  //chooses the one with the load for a particular request.
  //Although, is this optimization needed? And what if the request
  //is chasing a bandwidth, can this be done reliably?  
  public static function getNetworkPeers(config : RPCConfig) : List<PeerPair> { 
    var httpRequest : Http = 
      config.getHttpWithPath("/network/peers");
    httpRequest.request();
    trace("Get network peers");
    var dynArray : Array<Dynamic> = haxe.Json.parse(httpRequest.responseData);
    var res : List<PeerPair> = new List();
    for(aDyn in dynArray) {
      trace('$aDyn');
      res.add(new PeerPair(aDyn[0], Peer.parseJSON(aDyn[1])));
    }
    return res;
  }
  public static function getNetworkPeer(config : RPCConfig, peerId : PeerId) :
    PeerPair {
      var httpRequest : Http =
        config.getHttpWithPath('/network/peers/$peerId');
      httpRequest.request();
      trace('Get network information for $peerId');
      var dynPeer : Dynamic = 
        haxe.Json.parse(httpRequest.responseData);
      trace(dynPeer);
      var peerInfo : PeerPair = 
        new PeerPair(peerId, Peer.parseJSON(dynPeer));
      return peerInfo;
    }
  /**
  * Ban the peer with $peerId.
  */
  public static function banNetworkPeer(config : RPCConfig, peer : PeerPair) {
    var peerId = peer.peerId;
    var httpRequest : Http =
      config.getHttpWithPath('/network/peers/$peerId/ban');
    httpRequest.request();
    trace('Banned $peer.peerId');
  }


  public static function checkPeerBanStatus(config : RPCConfig, peer : PeerPair) : Bool {
    var peerId = peer.peerId;    
    var httpRequest : Http = 
      config.getHttpWithPath('/network/peers/$peerId/banned');
    httpRequest.request();
    var responseData = httpRequest.responseData;
    trace('Ban status $httpRequest : $responseData');
    return (haxe.Json.parse(responseData));
  }

  //A none -> false. This should be ok?
  private static function convertOptionToBool(anOption : Option<Bool>) : Bool {
    switch(anOption) {
      case(Some(anOption)): return anOption;
      case None : return false;
    }
  }
  public static function monitorPeerLog(config : RPCConfig, 
      peer : PeerPair, monitor : Option<Bool>) : List<Monitor> {
    var peerId = peer.peerId;
    var httpRequest : Http =
      config.getHttpWithPath('/network/peers/$peerId/log');
    var monitorParameter : Bool = convertOptionToBool(monitor);
    var httpRequest1 : Http = httpRequest.setParameter("monitor", 
        '$monitorParameter');
    httpRequest1.request();
    var result : Array<Dynamic> = 
      haxe.Json.parse(httpRequest1.responseData);
    return (Monitor.parseFromJSONList(result));
  }
  /**
  * Trust peer 'peerPair' permanently. The host IP can
  * still be blocked. 
  */
 
  /* 
  * TODO: Under what conditions should a node trust a peer?
  * Does it have any performance benefits?
  * Can a vpn be used to manage the peers so that all peers 
  * inside the vpn can be trusted as the start? Is this 
  * how to setup nodes? 
  */
  public static function trustPeer(config : RPCConfig, 
      peerPair : PeerPair) {
    var peerId = peerPair.peerId;
    var httpRequest : Http =
      config.getHttpWithPath('/network/peers/$peerId/trust');
    trace('Note: IP for the host can still be blocked. Trust $peerPair. ');
    httpRequest.request();
  }


  /**
  * List the pool of known IP, Port used for establishing P2P connections.
  * 
  */
  public static function getNetworkPoints (config : RPCConfig, 
    stateFilter : Option<StateFilter>) : List<PointPair> {
    var httpRequest0 : Http = 
      config.getHttpWithPath('/network/points/');
    var httpRequest : Http = 
      setOptionalParameters(stateFilter, "filter", httpRequest0);
    httpRequest0.request();
    var dynArray : Array<Dynamic> = haxe.Json.parse(httpRequest.responseData);
    var res : List<PointPair> = new List();
    for(aDyn in dynArray) {
      trace('$aDyn');
      res.add(new PointPair(aDyn[0], Point.parseJSON(aDyn[1])));
    }
    return res;
  }

  public static function getNetworkPoint(config : RPCConfig, point : String) : PointPair {
    var ipAddr  = point;
    trace('Network point $point');
    var url = "/network/points/" + "[" + ipAddr + "]";
    trace('$url');
    var httpRequest : Http = 
      config.getHttpWithPath(url);
    httpRequest.request();
    var responseD = httpRequest.responseData;
    trace(" " + responseD);
    var dyn : Dynamic = haxe.Json.parse(responseD);

    return (new PointPair(dyn[1], Point.parseJSON(dyn[0])));
  }

  public static function connectToPeer(config : RPCConfig, point : String) {
    var ipAddr = point;
    throw "PUT request not supported";       
  }

  public static function banPeer(config : RPCConfig, point : String) {
    var url = "/network/points/" + point + "/ban";
    var httpRequest : Http =
      config.getHttpWithPath(url);
    httpRequest.request();
    trace("Banned " + point);
  }
  public static function checkBannedStatus(config : RPCConfig, point : String) : Bool  {
    var url = "/network/points/" + point + "/banned";
    var httpRequest : Http = 
      config.getHttpWithPath(url);
    httpRequest.request();
    return (haxe.Json.parse(httpRequest.responseData));
  }
  public static function getPeerLog(config : RPCConfig, point : String) {
    throw ("Feature not supported. TODO");
  }

  /**
  * Need to model this in tlaplus. 
  * Trust this peer permanently. 
  * If state is trusted at a time: 
  *   connection is open.
  *   connection is closed, implies that 
  *     the connection is blacklisted or grey listed until a time.
  */
  public static function trustPoint (config : RPCConfig, point : String) {
    var url = "/network/points/" + point + "/trust";
    var httpRequest : Http = 
      config.getHttpWithPath(url);
    httpRequest.request();

  }

  public static function networkStat(config : RPCConfig) : PeerStatistics {
    var url = '/network/stat';
    var httpRequest : Http =
      config.getHttpWithPath(url);
    httpRequest.request();
    var result = httpRequest.responseData;
    return (PeerStatistics.parseJSON(result));
  }
  
  public static function getNetworkVersions(config : RPCConfig) : List<ConnectionVersion> {
    var url = '/network/versions';
    var httpRequest : Http = 
      config.getHttpWithPath(url);
    httpRequest.request();
    var responses : Array<Dynamic> = haxe.Json.parse(httpRequest.responseData);
    return (ConnectionVersion.fromArray(responses));
  }

  public static function getProtocols(config : RPCConfig) : List<Protocol> {
    var url = '/protocols';
    var httpRequest : Http = 
      config.getHttpWithPath(url);
    httpRequest.request();
    var responses : Array<String> = haxe.Json.parse(httpRequest.responseData);
    var result = new List();
    for (response in responses) {
      result.add(response);
    }
    return result;
  }

  public static function getProtocol(config : RPCConfig, protocol : ProtocolHash) : InjectProtocol {
    var protoHash = protocol.hash;
    var url = '/protocols/$protoHash';
    trace(url);
    var httpRequest :Http = 
      config.getHttpWithPath(url);
    httpRequest.request();
    var result : Dynamic = httpRequest.responseData;
    return result;
  }

  //There is a single block validator per node.
  public static function introspectBlockValidator(config : RPCConfig) : BlockValidator {
    var url = '/workers/block_validator';
    var httpRequest : Http = 
      config.getHttpWithPath(url);
    httpRequest.request();
    trace(httpRequest.responseData);
    var validator : Dynamic = haxe.Json.parse(httpRequest.responseData);
    return (new BlockValidator(validator));
  }  

  //There can be multiple chain validators per node.
  public static function introspectChainValidators(config : RPCConfig) : 
    List<ChainValidator> {
    var url = 'workers/chain_validators';
    var httpRequest : Http = 
      config.getHttpWithPath(url);
    httpRequest.request();
    trace('Chain validators ${httpRequest.responseData}');
    var validators : Array<Dynamic> = 
      haxe.Json.parse(httpRequest.responseData);
    return (ChainValidator.fromJSONArray(validators));
  }

  public static function introspectChainPrevalidators(config : RPCConfig) : 
    List<ChainValidator> {
    var url = 'workers/prevalidators';
    var httpRequest : Http = 
      config.getHttpWithPath(url);
    httpRequest.request();
    trace('Chain validators ${httpRequest.responseData}');
    var validators : Array<Dynamic> = 
      haxe.Json.parse(httpRequest.responseData);
    return (ChainValidator.fromJSONArray(validators));
  }

  public static function getBlockDetails(config : RPCConfig, chainId : String, blockId : String) : Dynamic {
    var url = '/chains/$chainId/blocks/$blockId';
    var httpRequest : Http = config.getHttpWithPath(url);
    trace('$httpRequest');
    httpRequest.request();
    trace('Block details : ${httpRequest.responseData}');
    return httpRequest.responseData;
  }

  public static function getBlockContextConstants(config : RPCConfig, 
    chainId : String, blockId : String) : ContextConstants {
    var url = '/chains/$chainId/blocks/$blockId/head/context/constants';
    var httpRequest : Http = config.getHttpWithPath(url);
    httpRequest.request();
    trace('$httpRequest : Response ${httpRequest.responseData}');
    return (new ContextConstants(haxe.Json.parse(httpRequest.responseData)));
  }
}


  
//TODO: These types need to be consolidated. 
//Notes: Implement the rpc client in haskell and 
//get a better sense of the type system.
class ValidBlockSummary {
  public var chainId(default, null) : ChainId;
  public var hash (default, null) : BlockHash;
  public function new (dyn : Dynamic) {
    chainId = new ChainId(dyn.chain_id);
    hash = new BlockHash(dyn.hash);
  }

}
