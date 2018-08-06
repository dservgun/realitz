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
import haxe.io.Bytes;
import haxe.Http;
import haxe.ds.Option;
import realitz.tezos.rpc.encoding.Requests.InjectBlock;
import realitz.tezos.rpc.encoding.Requests.InjectOperation;
import realitz.tezos.rpc.encoding.Requests.InjectProtocol;
import realitz.tezos.rpc.encoding.Responses.InvalidBlockResponse;
import realitz.tezos.rpc.encoding.Responses.Bootstrapped;
import realitz.tezos.rpc.encoding.Responses.Mempool;

class RPCConfig {
  public var url (default, null) : String;
  public var port (default, null) : String;
  public function getHttp() : Http {
    return (new Http('$url:$port'));
  }
  public function getHttpWithPath(path : String) : Http {
    return (new Http('$url:$port/$path'));
  }

  public function new (aUrl : String, aPort : String) {
    url = aUrl;
    port = aPort;
  }

}

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
    if (res != null) {
      var result = haxe.Json.parse(res);
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

  static function getInvalidBlocks(config : RPCConfig, chainId : String) : List<InvalidBlockResponse> {
    var httpRequest : Http = config.getHttpWithPath("chains/$chainId/invalid_blocks");
    httpRequest.request();
    var res : Null<String> = httpRequest.responseData;
    if (res != null) {
      return (haxe.Json.parse(res));
    }else {
      return (new List());
    }
  }

  static function getInvalidBlock(config : RPCConfig, chainId : String, blockHash : String) : Option<InvalidBlockResponse> {
    var httpRequest : Http = config.getHttpWithPath(
      "chains/$chainId/invalid_blocks/$blockkHash"
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

  static function getMempool (config : RPCConfig, chainId : String) : Option<Mempool> {
    var httpRequest : Http = config.getHttpWithPath(
      "chains/$chainId/mempool"
    );
    httpRequest.request();
    var res : Null<String> = httpRequest.responseData;
    if (res != null) {
      var result : Dynamic = haxe.Json.parse(res);
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
  static function monitorBootstrappedBlocks(config : RPCConfig) : Option<Bootstrapped> {
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
  static function monitorHeadBlockForChain (config : RPCConfig, chainId : Option<ChainId>, nextProtocol : Option<ProtocolHash>) : Option<BlockHash> {
    var httpRequestO : Http = config.getHttpWithPath("monitor/heads/$cId");
    var httpRequest1 : Http = setNextProtocol(nextProtocol, httpRequestO);
    var httpRequest : Http = setChainIdParameter(chainId, httpRequest1);
    httpRequest.request();
    var res : Null<String> = httpRequest.responseData;
    if (res != null) {
      return (Some (new BlockHash(res)));
    }else {
      return None;
    }
  }

  //TODO : FIX ME (the documentation from the main source).
  static function monitorProtocols (config : RPCConfig) : Option<BlockHash> {
    var httpRequest : Http = config.getHttpWithPath("monitor/protocols");
    httpRequest.request();
    var res : Null<String> = httpRequest.responseData;
    if (res != null) {
      return (Some (new BlockHash(res)));
    }else {
      return None;
    }
  }

  static function monitorValidBlocks(config : RPCConfig, protocol : Option<ProtocolHash>, nextProtocol : Option<ProtocolHash>, 
  chainId : Option<ChainId>) : Option<ValidBlockSummary> {
    var httpRequest : Http = config.getHttpWithPath("monitor/valid_blocks");
    var httpRequest0 : Http = setProtocol(protocol, httpRequest);
    var httpRequest1 : Http = setNextProtocol(protocol, httpRequest0);
    var httpRequest2 : Http = setChainIdParameter(chainId, httpRequest1);
    httpRequest2.request();
    var res : Null<String> = httpRequest.responseData;
    if (res != null){
      var dyn : Dynamic = haxe.Json.parse(res);
      return Some(new ValidBlockSummary(dyn));
    }else {
      return None;
    }
  }

  /** Network operations */
  static function getNetworkConnections(config : RPCConfig) : 
    List<ConnectionInformation> {
      var httpRequest : Http = config.getHttpWithPath("network/connections");
      httpRequest.request();
      var res : Null<String> = httpRequest.responseData;
      if (res != null) {
        var dyn : List<Dynamic> = haxe.Json.parse(res);
        return (ConnectionInformation.parseJSON(dyn));
      }else {
        return (new List());
      }
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
