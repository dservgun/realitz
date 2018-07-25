package realitz.tezos.rpc;

import haxe.io.Bytes;
import haxe.Int64;

/** 
Code walkthrough process: it is simpler and probably intended that 
as implementers of client api, we assume types specified in the 
.mli (ml interface files). This ensures that during client implementations
we inadvertently peek into server implementation.
Block header and the header with data refer to the 
types defined here : file : block_header.mli.

type shell_header = {
  level: Int32.t ;
  proto_level: int ; (* uint8 *)
  predecessor: Block_hash.t ;
  timestamp: Time.t ;
  validation_passes: int ; (* uint8 *)
  operations_hash: Operation_list_list_hash.t ;
  fitness: Fitness.t ;
  context: Context_hash.t ;
}

val shell_header_encoding: shell_header Data_encoding.t

type t = {
  shell: shell_header ;
  protocol_data: MBytes.t ;
}

*/

class BlockHeader {
  var level :  Int;
  var proto : Int ;
  var predecessor : Bytes ;
  var timestamp : Bytes;
  var validationPass : Int; 
  var operationsHash : Array<Array<Bytes>>;
  var fitness : Array<Bytes>; 
  var context : Bytes;
}


class BlockHeaderWithData {
  var header : BlockHeader; 
  /* 
  * TODO: Bytes types wraps platform dependent bytes. 
  * Check on irc if this is the right way to doing this.
  */ 
  var data : Bytes;
}

class BlockHash {
  var hash : String;
}

class ChainId {
  var hash : String;
}

class BlockResponse {
  var block : BlockHash; 
  var level : Int ;
  var errors : List<Error>;
}

class InjectBlock {
  var data : Bytes;
  var operations : 
    List <List < {branch : BlockHash, data : Bytes} > >;
}

class InjectOperation {
  var data : Bytes;
}

class OperationHash {
  var hash : String;
}

class Component {
  var name : String ;
  var _interface : Bytes;
  var implementation : Bytes;
}

class ProtocolHash { 
  var hash : String;
}

class ContextHash {
  var hash : String;
}
class InjectProtocol {
  var expectedEnvVersion : Int;
  var components : List<Component>;
} 

class Bootstrapped {
  var block : BlockHash ;
  var timestamp : Int64;
}

class Fitness {
  var fitness : List<Bytes>;
}

class PublicKey {
  var hash : String;
}

class BlockInformation {
  var chainId  : ChainId;
  var hash : BlockHash;
  var level : Int; 
  var proto : Int;
  var predecessor : BlockHash ;
  var timestamp : Int64;
  var validationPass : Int;
  var operationsHash : OperationHash;
  var fitness : Fitness;
  var context : ContextHash; 
  var protocolData : Bytes;
}

class IdPoint {
  var address : String;
  var port : Int;
}
class ConnectionInformation {
  var incoming : Bool;
  var peerId : PublicKey;
  var idPoint : IdPoint;
}