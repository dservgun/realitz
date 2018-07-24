package realitz.tezos.rpc;

/*
       (req "level" int32)
       (req "proto" uint8)
       (req "predecessor" Block_hash.encoding)
       (req "timestamp" Time.encoding)
       (req "validation_pass" uint8)
       (req "operations_hash" Operation_list_list_hash.encoding)
       (req "fitness" Fitness.encoding)
       (req "context" Context_hash.encoding))

*/

class BlockHeader {
  var level :  Int;
  var proto : Int ;
  var predecessor : String ;
  var timestamp : String;
  var validationPass : Int; 
  var operationsHash : Array<Array<String>>;
  var fitness : Array<String>; 
  var context : String;
}