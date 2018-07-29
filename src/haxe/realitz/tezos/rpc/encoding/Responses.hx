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
import haxe.Int64;
import haxe.io.Bytes;


class ChainIdResponse {
  var response (default, null) : ChainId;
}

class InvalidBlockResponse {
  var response (default, null) : BlockResponse;
}

class Bootstrapped {
  var block : BlockHash ;
  var timestamp : Int64;
}



class Fitness {
  var fitness : List<Bytes>;
  public function new (jsonString : String) {
    fitness = haxe.Json.parse(jsonString);
  }
}

class MonitorBlock {
  var hash : BlockHash;
  var level : Int; 
  var proto : Int;
  var predecessor : BlockHash ;
  var timestamp : Date;
  var validationPass : Int;
  var operationsHash : OperationHash;
  var fitness : Fitness;
  var context : ContextHash; 
  var protocolData : Bytes;
  function new (jsonString : String) {
    var json : Dynamic = haxe.Json.parse(jsonString);
    hash = json.hash;
    level = json.level;
    proto = json.proto;
    predecessor = new BlockHash(json.predecessor);
    timestamp = haxe.Json.parse(json.timestamp);
    validationPass = json.validation_pass;
    operationsHash = haxe.Json.parse(json.operations_hash);
    fitness = new Fitness(  json.fitness);
    context = new ContextHash (json.context);
    protocolData = json.protocol_data;
  }
}

class ValidBlock {
  var chainId : ChainId;
  var hash : BlockHash;
  var level : Int; 
  var proto : Int;
  var predecessor : BlockHash ;
  var timestamp : Date;
  var validationPass : Int;
  var operationsHash : OperationHash;
  var fitness : Fitness;
  var context : ContextHash; 
  var protocolData : Bytes;
  function new (jsonString : String) {
    var json : Dynamic = haxe.Json.parse(jsonString);
    chainId = new ChainId(json.chain_id);
    hash = json.hash;
    level = json.level;
    proto = json.proto;
    predecessor = new BlockHash(json.predecessor);
    timestamp = haxe.Json.parse(json.timestamp);
    validationPass = json.validation_pass;
    operationsHash = haxe.Json.parse(json.operations_hash);
    fitness = new Fitness(  json.fitness);
    context = new ContextHash (json.context);
    protocolData = json.protocol_data;
  }  
}

