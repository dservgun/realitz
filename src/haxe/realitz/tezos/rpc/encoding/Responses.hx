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



class InvalidBlockResponse {
  var block (default, null) : BlockHash; 
  var level (default, null) : Int ;
  var errors (default, null) : List<Error>;
}

class Bootstrapped {
  var block : BlockHash ;
  var timestamp : Int64;
  public function new (dyn : Dynamic) {
    block = dyn.block;
    timestamp = dyn.timestamp;
  }
  public static function fromJSON(aString : String) : Bootstrapped  {
    var dyn : Dynamic = haxe.Json.parse(aString);
    return (new Bootstrapped(dyn));

  }
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

//TODO: Create the appropriate type.
typedef ErrorDyn = Dynamic;
enum MempoolType {
  Applied (hash : String, branch : String, data : Bytes);
  Refused (hash : String, branch : String, data : Bytes, error : List<ErrorDyn>);
  BranchRefused (hash : String, branch : String, data : Bytes, error : List<ErrorDyn>);
  BranchDelayed (hash : String, branch : String, data : Bytes, error : List<ErrorDyn>);
  Unprocessed (hash : String, branch : String, data : Bytes);
}

class Mempool {
  function new (
    app : List<MempoolType>,
    refusedL : List<MempoolType>,
    branchRefusedL : List<MempoolType>,
    branchDelayedL : List<MempoolType>,
    unprocessedL : List<MempoolType> 
    ) {
    applied = app;
    refused = refusedL;
    branchRefused = branchRefusedL;
    branchDelayed = branchDelayedL;
    unprocessed = unprocessedL;
  } 
  public var applied (default, null) : List<MempoolType>;
  public var refused (default, null): List<MempoolType>;
  public var branchRefused (default, null) : List<MempoolType>;
  public var branchDelayed (default, null) : List<MempoolType>;
  public var unprocessed (default, null) : List<MempoolType>;
  private static function appliedMem(appliedMempool : Iterator<Dynamic>) : List<MempoolType> {
    var result : List<MempoolType> = new List();
    for (aVal in appliedMempool) {
      result.add(Applied(aVal.hash, aVal.branch, aVal.data));
    }
    return result;
  }
  private static function refusedMem(refusedMem : Iterator<Dynamic>) : List<MempoolType> {
    var result : List<MempoolType> = new List();
    for (aVal in refusedMem) {
      result.add(Refused(aVal.hash, aVal.branch, aVal.data, aVal.errors));
    }
    return result;
  }

  private static function branchRefusedMem(mempool : Iterator<Dynamic>) : List<MempoolType> {
    var result : List<MempoolType> = new List();
    for (aVal in mempool) {
      result.add(BranchRefused(aVal.hash, aVal.branch, aVal.data, aVal.errors));
    }
    return result;
  }

  private static function branchDelayedMem(mempool : Iterator<Dynamic>) : List<MempoolType> {
    var result : List<MempoolType> = new List();
    for (aVal in mempool) {
      result.add(BranchDelayed(aVal.hash, aVal.branch, aVal.data, aVal.errors));
    }
    return result;
  }
  private static function unprocessedMem (mempool : Iterator<Dynamic>) : List<MempoolType> {
    var result : List<MempoolType> = new List();
    for (aVal in mempool) {
      result.add(Unprocessed(aVal.hash, aVal.branch, aVal.data));
    }
    return result;
  }

  var pool (default, null) : List<MempoolType>;
  public static function fromDynamic(mempoolResponse : Dynamic) : Mempool {
    var applied : List<MempoolType> = appliedMem(mempoolResponse.applied.iterator());
    var refused : List<MempoolType> = 
        refusedMem(mempoolResponse.refused.iterator());
    var branchRefused : List<MempoolType> = 
      branchRefusedMem(mempoolResponse.branch_refused.iterator());
    var branchDelayed : List<MempoolType> = 
      branchDelayedMem(mempoolResponse.branch_delayed.iterator());
    var unprocessed : List<MempoolType>  = 
      unprocessedMem(mempoolResponse.unprocessed.iterator());
    return (new Mempool(applied, refused, branchRefused, branchDelayed, unprocessed));
    
  }
}
