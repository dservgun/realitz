package realitz.tezos.rpc;

import haxe.io.Bytes;

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