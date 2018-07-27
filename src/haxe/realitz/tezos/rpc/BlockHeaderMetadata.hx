package realitz.tezos.rpc;

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

import realitz.tezos.rpc.Types;
import realitz.tezos.rpc.WorkerTypes;
import haxe.Int64;

/*

type t =
  | Not_running
  | Forking of {
      protocol: Protocol_hash.t ;
      expiration: Time.t ;
    }
  | Running of {
      chain_id: Chain_id.t ;
      genesis: Block_hash.t ;
      protocol: Protocol_hash.t ;
      expiration: Time.t ;
    }

*/

typedef Protocol = String;
enum TestChainStatus {
  NotRunning;
  Forking (protocol : Protocol, expiration : Time);
  Running (chainId : ChainId, genesis : BlockHash, protocol : Protocol, expiration : Time);
}


/*
TODO: Ask someone, where in the core code is the structure or the interface defined 
for the metadata. Currently using the json schema. It works, but it will be nice 
to tie to the file in the code that publishes the json.

*/
class BlockHeaderMetadata {
  var protocol : ProtocolHash;
  var nextProtocol : ProtocolHash;
  var testChainStatus : TestChainStatus;
  var maxOperationsTTL : Int;
  var maxOperationDataLength : Int;
  var maxOperationListLength : Int;
  var baker : Baker;
  var level : Level;
  var votingPeriodKind : VotingPeriodKind;
}


class Level {
  var level : Int;
  var levelPosition : Int;
  var cycle : Int;
  var cyclePosition : Int;
  var votingPeriod : Int;
  var votingPeriodPosition : Int;
  var expectedCommitment : Bool;
  var nonceHash : BlockHash; 
  var consumedGas : Int64; // TODO : need to see if this will suffice. Spec says bignum.
  var deactivated : BlockHash;
  var balanceUpdates : List<BalanceUpdate>;
}

enum VotingPeriodKind {
  Proposal;
  TestingVote;
  Testing;
  PromotionVote;
}
/*
 { "protocol": "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY",
    "chain_id": $Chain_id,
    "hash": $block_hash,
    "header": $raw_block_header,
    "metadata": $block_header_metadata,
    "operations": [ [ $operation ... ] ... ] }
  $Chain_id:  string
  $Context_hash: string
  $Ed25519.Public_key_hash: string
  $Operation_hash: string
  $Operation_list_list_hash: string
  $Protocol_hash: string
  $Signature:string
  $Signature.Public_key:string
  $Signature.Public_key_hash:string
  $bignum:string
  $block_hash:string
  $block_header.alpha.full_header:
    { "level": integer ∈ [-2^31-2, 2^31+2],
      "proto": integer ∈ [0, 255],
      "predecessor": $block_hash,
      "timestamp": $timestamp,
      "validation_pass": integer ∈ [0, 255],
      "operations_hash": $Operation_list_list_hash,
      "fitness": $fitness,
      "context": $Context_hash,
      "priority": integer ∈ [0, 2^16-1],
      "proof_of_work_nonce": /^[a-zA-Z0-9]+$/,
      "seed_nonce_hash"?: $cycle_nonce,
      "signature": $Signature }
  $block_header_metadata:
    { "protocol": "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY",
      "next_protocol": "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY",
      "test_chain_status": $test_chain_status,
      "max_operations_ttl": integer ∈ [-2^30-2, 2^30+2],
      "max_operation_data_length": integer ∈ [-2^30-2, 2^30+2],
      "max_block_header_length": integer ∈ [-2^30-2, 2^30+2],
      "max_operation_list_length":
        [ { "max_size": integer ∈ [-2^30-2, 2^30+2],
            "max_op"?: integer ∈ [-2^30-2, 2^30+2] } ... ],
      "baker": $Signature.Public_key_hash,
      "level":
        { "level": integer ∈ [-2^31-2, 2^31+2],
          "level_position": integer ∈ [-2^31-2, 2^31+2],
          "cycle": integer ∈ [-2^31-2, 2^31+2],
          "cycle_position": integer ∈ [-2^31-2, 2^31+2],
          "voting_period": integer ∈ [-2^31-2, 2^31+2],
          "voting_period_position": integer ∈ [-2^31-2, 2^31+2],
          "expected_commitment": boolean },
      "voting_period_kind":
        "proposal" || "testing_vote" || "testing" || "promotion_vote",
      "nonce_hash": $cycle_nonce || null,
      "consumed_gas": $positive_bignum,
      "deactivated": [ $Signature.Public_key_hash ... ],
      "balance_updates": $operation_metadata.alpha.balance_updates }
  $contract_id:string
  $cycle_nonce:string
  $error:any
  $fitness:[ /^[a-zA-Z0-9]+$/ ... ]
  $inlined.endorsement:
    { "branch": $block_hash,
      "operations": $inlined.endorsement.contents,
      "signature"?: $Signature }
  $inlined.endorsement.contents:
    { "kind": "endorsement",
      "level": integer ∈ [-2^31-2, 2^31+2] }
  $int64:
    string
  $micheline.michelson_v1.expression:
    { "int": $bignum }
    || { "string": string }
    || { "bytes": /^[a-zA-Z0-9]+$/ }
    || [ $micheline.michelson_v1.expression ... ]
    || { "prim": $michelson.v1.primitives,
         "args"?: [ $micheline.michelson_v1.expression ... ],
         "annots"?: [ string ... ] }
  $michelson.v1.primitives:
    "ADD"
    | "LE"
    | "UPDATE"
    | "unit"
    | "string"
    | "COMPARE"
    | "LAMBDA"
    | "LOOP"
    | "Elt"
    | "IMPLICIT_ACCOUNT"
    | "NONE"
    | "signature"
    | "set"
    | "mutez"
    | "BLAKE2B"
    | "SHA256"
    | "ITER"
    | "bool"
    | "MAP"
    | "UNIT"
    | "DIP"
    | "PACK"
    | "pair"
    | "SIZE"
    | "Right"
    | "map"
    | "IF_CONS"
    | "LSR"
    | "SET_DELEGATE"
    | "storage"
    | "XOR"
    | "CDR"
    | "TRANSFER_TOKENS"
    | "SOME"
    | "False"
    | "SHA512"
    | "CHECK_SIGNATURE"
    | "BALANCE"
    | "lambda"
    | "operation"
    | "EMPTY_SET"
    | "SWAP"
    | "MEM"
    | "RIGHT"
    | "CONTRACT"
    | "or"
    | "CONCAT"
    | "nat"
    | "bytes"
    | "Unit"
    | "Some"
    | "UNPACK"
    | "NOT"
    | "LEFT"
    | "timestamp"
    | "AMOUNT"
    | "DROP"
    | "ABS"
    | "contract"
    | "GE"
    | "PUSH"
    | "LT"
    | "address"
    | "NEQ"
    | "NEG"
    | "None"
    | "CONS"
    | "EXEC"
    | "NIL"
    | "CAST"
    | "MUL"
    | "ADDRESS"
    | "EDIV"
    | "STEPS_TO_QUOTA"
    | "SUB"
    | "INT"
    | "SOURCE"
    | "CAR"
    | "CREATE_ACCOUNT"
    | "LSL"
    | "OR"
    | "IF_NONE"
    | "SELF"
    | "IF"
    | "Left"
    | "int"
    | "big_map"
    | "SENDER"
    | "option"
    | "DUP"
    | "EQ"
    | "NOW"
    | "key_hash"
    | "GET"
    | "list"
    | "key"
    | "True"
    | "GT"
    | "parameter"
    | "IF_LEFT"
    | "FAILWITH"
    | "PAIR"
    | "LOOP_LEFT"
    | "Pair"
    | "RENAME"
    | "EMPTY_MAP"
    | "CREATE_CONTRACT"
    | "HASH_KEY"
    | "ISNAT"
    | "code"
    | "AND"
  $mutez: $positive_bignum
  $operation:
    { "protocol": "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY",
      "chain_id": $Chain_id,
      "hash": $Operation_hash,
      "branch": $block_hash,
      "contents": [ $operation.alpha.operation_contents_and_result ... ],
      "signature"?: $Signature }
    || { "protocol": "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY",
         "chain_id": $Chain_id,
         "hash": $Operation_hash,
         "branch": $block_hash,
         "contents": [ $operation.alpha.contents ... ],
         "signature"?: $Signature }
  $operation.alpha.contents:
    { "kind": "endorsement",
      "level": integer ∈ [-2^31-2, 2^31+2] }
    || { "kind": "seed_nonce_revelation",
         "level": integer ∈ [-2^31-2, 2^31+2],
         "nonce": /^[a-zA-Z0-9]+$/ }
    || { "kind": "double_endorsement_evidence",
         "op1": $inlined.endorsement,
         "op2": $inlined.endorsement }
    || { "kind": "double_baking_evidence",
         "bh1": $block_header.alpha.full_header,
         "bh2": $block_header.alpha.full_header }
    || { "kind": "activate_account",
         "pkh": $Ed25519.Public_key_hash,
         "secret": /^[a-zA-Z0-9]+$/ }
    || { "kind": "proposals",
         "source": $Signature.Public_key_hash,
         "period": integer ∈ [-2^31-2, 2^31+2],
         "proposals": [ $Protocol_hash ... ] }
    || { "kind": "ballot",
         "source": $Signature.Public_key_hash,
         "period": integer ∈ [-2^31-2, 2^31+2],
         "proposal": $Protocol_hash,
         "ballot": "nay" | "yay" | "pass" }
    || { "kind": "reveal",
         "source": $contract_id,
         "fee": $mutez,
         "counter": $positive_bignum,
         "gas_limit": $positive_bignum,
         "storage_limit": $positive_bignum,
         "public_key": $Signature.Public_key }
    || { "kind": "transaction",
         "source": $contract_id,
         "fee": $mutez,
         "counter": $positive_bignum,
         "gas_limit": $positive_bignum,
         "storage_limit": $positive_bignum,
         "amount": $mutez,
         "destination": $contract_id,
         "parameters"?: $micheline.michelson_v1.expression }
    || { "kind": "origination",
         "source": $contract_id,
         "fee": $mutez,
         "counter": $positive_bignum,
         "gas_limit": $positive_bignum,
         "storage_limit": $positive_bignum,
         "managerPubkey": $Signature.Public_key_hash,
         "balance": $mutez,
         "spendable"?: boolean,
         "delegatable"?: boolean,
         "delegate"?: $Signature.Public_key_hash,
         "script"?: $scripted.contracts }
    || { "kind": "delegation",
         "source": $contract_id,
         "fee": $mutez,
         "counter": $positive_bignum,
         "gas_limit": $positive_bignum,
         "storage_limit": $positive_bignum,
         "delegate"?: $Signature.Public_key_hash }
  $operation.alpha.internal_operation_result:
    { "kind": "reveal",
      "source": $contract_id,
      "nonce": integer ∈ [0, 2^16-1],
      "public_key": $Signature.Public_key,
      "result": $operation.alpha.operation_result.reveal }
    || { "kind": "transaction",
         "source": $contract_id,
         "nonce": integer ∈ [0, 2^16-1],
         "amount": $mutez,
         "destination": $contract_id,
         "parameters"?: $micheline.michelson_v1.expression,
         "result": $operation.alpha.operation_result.transaction }
    || { "kind": "origination",
         "source": $contract_id,
         "nonce": integer ∈ [0, 2^16-1],
         "managerPubkey": $Signature.Public_key_hash,
         "balance": $mutez,
         "spendable"?: boolean,
         "delegatable"?: boolean,
         "delegate"?: $Signature.Public_key_hash,
         "script"?: $scripted.contracts,
         "result": $operation.alpha.operation_result.origination }
    || { "kind": "delegation",
         "source": $contract_id,
         "nonce": integer ∈ [0, 2^16-1],
         "delegate"?: $Signature.Public_key_hash,
         "result": $operation.alpha.operation_result.delegation }
  $operation.alpha.operation_contents_and_result:
    { "kind": "endorsement",
      "level": integer ∈ [-2^31-2, 2^31+2],
      "metadata":
        { "balance_updates": $operation_metadata.alpha.balance_updates,
          "delegate": $Signature.Public_key_hash,
          "slots": [ integer ∈ [0, 255] ... ] } }
    || { "kind": "seed_nonce_revelation",
         "level": integer ∈ [-2^31-2, 2^31+2],
         "nonce": /^[a-zA-Z0-9]+$/,
         "metadata":
           { "balance_updates": $operation_metadata.alpha.balance_updates } }
    || { "kind": "double_endorsement_evidence",
         "op1": $inlined.endorsement,
         "op2": $inlined.endorsement,
         "metadata":
           { "balance_updates": $operation_metadata.alpha.balance_updates } }
    || { "kind": "double_baking_evidence",
         "bh1": $block_header.alpha.full_header,
         "bh2": $block_header.alpha.full_header,
         "metadata":
           { "balance_updates": $operation_metadata.alpha.balance_updates } }
    || { "kind": "activate_account",
         "pkh": $Ed25519.Public_key_hash,
         "secret": /^[a-zA-Z0-9]+$/,
         "metadata":
           { "balance_updates": $operation_metadata.alpha.balance_updates } }
    || { "kind": "proposals",
         "source": $Signature.Public_key_hash,
         "period": integer ∈ [-2^31-2, 2^31+2],
         "proposals": [ $Protocol_hash ... ],
         "metadata": {  } }
    || { "kind": "ballot",
         "source": $Signature.Public_key_hash,
         "period": integer ∈ [-2^31-2, 2^31+2],
         "proposal": $Protocol_hash,
         "ballot": "nay" | "yay" | "pass",
         "metadata": {  } }
    || { "kind": "reveal",
         "source": $contract_id,
         "fee": $mutez,
         "counter": $positive_bignum,
         "gas_limit": $positive_bignum,
         "storage_limit": $positive_bignum,
         "public_key": $Signature.Public_key,
         "metadata":
           { "balance_updates": $operation_metadata.alpha.balance_updates,
             "operation_result": $operation.alpha.operation_result.reveal,
             "internal_operation_results"?:
               [ $operation.alpha.internal_operation_result ... ] } }
    || { "kind": "transaction",
         "source": $contract_id,
         "fee": $mutez,
         "counter": $positive_bignum,
         "gas_limit": $positive_bignum,
         "storage_limit": $positive_bignum,
         "amount": $mutez,
         "destination": $contract_id,
         "parameters"?: $micheline.michelson_v1.expression,
         "metadata":
           { "balance_updates": $operation_metadata.alpha.balance_updates,
             "operation_result":
               $operation.alpha.operation_result.transaction,
             "internal_operation_results"?:
               [ $operation.alpha.internal_operation_result ... ] } }
    || { "kind": "origination",
         "source": $contract_id,
         "fee": $mutez,
         "counter": $positive_bignum,
         "gas_limit": $positive_bignum,
         "storage_limit": $positive_bignum,
         "managerPubkey": $Signature.Public_key_hash,
         "balance": $mutez,
         "spendable"?: boolean,
         "delegatable"?: boolean,
         "delegate"?: $Signature.Public_key_hash,
         "script"?: $scripted.contracts,
         "metadata":
           { "balance_updates": $operation_metadata.alpha.balance_updates,
             "operation_result":
               $operation.alpha.operation_result.origination,
             "internal_operation_results"?:
               [ $operation.alpha.internal_operation_result ... ] } }
    || { "kind": "delegation",
         "source": $contract_id,
         "fee": $mutez,
         "counter": $positive_bignum,
         "gas_limit": $positive_bignum,
         "storage_limit": $positive_bignum,
         "delegate"?: $Signature.Public_key_hash,
         "metadata":
           { "balance_updates": $operation_metadata.alpha.balance_updates,
             "operation_result": $operation.alpha.operation_result.delegation,
             "internal_operation_results"?:
               [ $operation.alpha.internal_operation_result ... ] } }
  $operation.alpha.operation_result.delegation:
    { "status": "applied" }
    || { "status": "failed",
         "errors": [ $error ... ] }
    || { "status": "skipped" }
    || { "status": "backtracked",
         "errors"?: [ $error ... ] }
  $operation.alpha.operation_result.origination:
    { "status": "applied",
      "balance_updates"?: $operation_metadata.alpha.balance_updates,
      "originated_contracts"?: [ $contract_id ... ],
      "consumed_gas"?: $bignum,
      "storage_size"?: $bignum,
      "paid_storage_size_diff"?: $bignum }
    || { "status": "failed",
         "errors": [ $error ... ] }
    || { "status": "skipped" }
    || { "status": "backtracked",
         "errors"?: [ $error ... ],
         "balance_updates"?: $operation_metadata.alpha.balance_updates,
         "originated_contracts"?: [ $contract_id ... ],
         "consumed_gas"?: $bignum,
         "storage_size"?: $bignum,
         "paid_storage_size_diff"?: $bignum }
  $operation.alpha.operation_result.reveal:
    { "status": "applied" }
    || { "status": "failed",
         "errors": [ $error ... ] }
    || { "status": "skipped" }
    || { "status": "backtracked",
         "errors"?: [ $error ... ] }
  $operation.alpha.operation_result.transaction:
    { "status": "applied",
      "storage"?: $micheline.michelson_v1.expression,
      "balance_updates"?: $operation_metadata.alpha.balance_updates,
      "originated_contracts"?: [ $contract_id ... ],
      "consumed_gas"?: $bignum,
      "storage_size"?: $bignum,
      "paid_storage_size_diff"?: $bignum }
    || { "status": "failed",
         "errors": [ $error ... ] }
    || { "status": "skipped" }
    || { "status": "backtracked",
         "errors"?: [ $error ... ],
         "storage"?: $micheline.michelson_v1.expression,
         "balance_updates"?: $operation_metadata.alpha.balance_updates,
         "originated_contracts"?: [ $contract_id ... ],
         "consumed_gas"?: $bignum,
         "storage_size"?: $bignum,
         "paid_storage_size_diff"?: $bignum }
  $operation_metadata.alpha.balance_updates:
    [ { "kind": "contract",
        "contract": $contract_id,
        "change": $int64 }
      || { "kind": "freezer",
           "category": "rewards",
           "delegate": $Signature.Public_key_hash,
           "level": integer ∈ [-2^31-2, 2^31+2],
           "change": $int64 }
      || { "kind": "freezer",
           "category": "fees",
           "delegate": $Signature.Public_key_hash,
           "level": integer ∈ [-2^31-2, 2^31+2],
           "change": $int64 }
      || { "kind": "freezer",
           "category": "deposits",
           "delegate": $Signature.Public_key_hash,
           "level": integer ∈ [-2^31-2, 2^31+2],
           "change": $int64 } ... ]
  $positive_bignum:string
  $raw_block_header:
    { "level": integer ∈ [-2^31-2, 2^31+2],
      "proto": integer ∈ [0, 255],
      "predecessor": $block_hash,
      "timestamp": $timestamp,
      "validation_pass": integer ∈ [0, 255],
      "operations_hash": $Operation_list_list_hash,
      "fitness": $fitness,
      "context": $Context_hash,
      "priority": integer ∈ [0, 2^16-1],
      "proof_of_work_nonce": /^[a-zA-Z0-9]+$/,
      "seed_nonce_hash"?: $cycle_nonce,
      "signature": $Signature }
  $scripted.contracts:
    { "code": $micheline.michelson_v1.expression,
      "storage": $micheline.michelson_v1.expression }
  $test_chain_status:
    { "status": "not_running" }
    || { "status": "forking",
         "protocol": $Protocol_hash,
         "expiration": $timestamp }
    || { "status": "running",
         "chain_id": $Chain_id,
         "genesis": $block_hash,
         "protocol": $Protocol_hash,
         "expiration": $timestamp }
  $timestamp: $timestamp.rfc || $int64
  $timestamp.rfc:
*/
/*
  $operation_metadata.alpha.balance_updates:
    [ { "kind": "contract",
        "contract": $contract_id,
        "change": $int64 }
      || { "kind": "freezer",
           "category": "rewards",
           "delegate": $Signature.Public_key_hash,
           "level": integer ∈ [-2^31-2, 2^31+2],
           "change": $int64 }
      || { "kind": "freezer",
           "category": "fees",
           "delegate": $Signature.Public_key_hash,
           "level": integer ∈ [-2^31-2, 2^31+2],
           "change": $int64 }
      || { "kind": "freezer",
           "category": "deposits",
           "delegate": $Signature.Public_key_hash,
           "level": integer ∈ [-2^31-2, 2^31+2],
           "change": $int64 } 
*/

typedef ContractId = BlockHash
enum ContractKind {
  Freezer;
}
enum BalanceUpdate {
  Contract(contractId : ContractId, change : Int64);
  Rewards(kind : ContractKind, deletage : PublicKey, level : Int, change : Int64);
  Fees (kind : ContractKind, delegate : PublicKey, level : Int, change : Int64);
  Deposits(kind : ContractKind, delegate : PublicKey, level : Int, change : Int64);
}