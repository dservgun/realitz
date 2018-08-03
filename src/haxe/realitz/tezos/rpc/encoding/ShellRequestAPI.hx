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
import realitz.tezos.rpc.encoding.Responses.InvalidBlockResponse;
import realitz.tezos.rpc.encoding.Responses.Mempool;

class RPCConfig {
  public var url (default, null) : String;
  public var port (default, null) : String;
  public function getHttp() : Http {
    return (new Http("$url:$port"));
  }
  public function getHttpWithPath(path : String) : Http {
    return (new Http("$url:$port/$path"));
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
  static function getBlocksForAChain(config : RPCConfig, chainId : String, 
    length : Option<Int>, 
    head  : Option<BlockHash>, 
    minDate : Option<Date>) : List<List<BlockHash>> {
    var httpRequest : Http =  config.getHttpWithPath("chains/$chainId/blocks");
    var httpRequest1 : Http = setLength(length, httpRequest);
    var httpRequest2 : Http = setHead(head, httpRequest1);
    var httpRequest3 : Http = setMinDate(minDate, httpRequest2);
    httpRequest3.request();
    var res : Null<String> = httpRequest3.responseData;
    if (res != null) {
      var result = haxe.Json.parse(res);
      return result;
    }else {
      return (new List());
    }
  }

  static function getChainId (config : RPCConfig, chainId : String) : Option<ChainId> {
    var httpRequest : Http = config.getHttpWithPath("chains/$chainId/chain_id");
    httpRequest.request();
    var res : Null<String> = httpRequest.responseData;
    if (res != null) {
      return (Some (new ChainId(haxe.Json.parse(res))));
    }else {
      return None;
    }
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

}