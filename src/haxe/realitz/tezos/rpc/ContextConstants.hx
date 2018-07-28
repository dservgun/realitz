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


package realitz.tezos.rpc;
import haxe.Int64;
import realitz.tezos.rpc.Types;

/*
  { "proof_of_work_nonce_size": integer ∈ [0, 255],
    "nonce_length": integer ∈ [0, 255],
    "max_revelations_per_block": integer ∈ [0, 255],
    "max_operation_data_length": integer ∈ [-2^30-2, 2^30+2],
    "preserved_cycles": integer ∈ [0, 255],
    "blocks_per_cycle": integer ∈ [-2^31-2, 2^31+2],
    "blocks_per_commitment": integer ∈ [-2^31-2, 2^31+2],
    "blocks_per_roll_snapshot": integer ∈ [-2^31-2, 2^31+2],
    "blocks_per_voting_period": integer ∈ [-2^31-2, 2^31+2],
    "time_between_blocks": [ $int64 ... ],
    "endorsers_per_block": integer ∈ [0, 2^16-1],
    "hard_gas_limit_per_operation": $bignum,
    "hard_gas_limit_per_block": $bignum,
    "proof_of_work_threshold": $int64,
    "tokens_per_roll": $mutez,
    "michelson_maximum_type_size": integer ∈ [0, 2^16-1],
    "seed_nonce_revelation_tip": $mutez,
    "origination_burn": $mutez,
    "block_security_deposit": $mutez,
    "endorsement_security_deposit": $mutez,
    "block_reward": $mutez,
    "endorsement_reward": $mutez,
    "cost_per_byte": $mutez,
    "hard_storage_limit_per_operation": $bignum }
*/
class ContextConstants {
  var proofOfWorkNonceSize (default, null) : Int;
  var nonceLength (default, null) : Int;
  var maxRevelationsPerBlock (default, null) : Int;
  var maxOperationDataLength (default, null) : Int;
  var preservedCycles (default, null) : Int;
  var blocksPerCycle (default, null) : Int;
  var blocksPerCommitment (default, null) : Int;
  var blocksPerRollSnapshot (default, null) : Int;
  var blocksPerVotingPeriod (default, null) : Int;
  var timeBetweenBlocks (default, null) : List<Int64>;
  var endorsersPerBlock (default, null) : Int;
  var hardGasLimitPerOperation (default, null) : BigNum;
  var hardGasLimitPerBlock (default, null) : BigNum;
  var proofOfWorkThreshold (default, null) : Int64;
  var tokensPerRoll (default, null) : Mutez;
  var michelsonMaximumTypevarSize (default, null) : Int;
  var seedNonceRevelationTip (default, null) : Mutez;
  var originationBurn (default, null) : Mutez;
  var blockSecurityDeposit (default, null) : Mutez;
  var endorsementSecurityDeposit (default, null) : Mutez;
  var blockReward (default, null) : Mutez;
  var endorsementReward (default, null) : Mutez;
  var costPerByte (default, null) : Mutez;
  var hardStorageLimitPerOperation (default, null) : BigNum;
  function new (aJsonString : String) {
    var json : Dynamic = haxe.Json.parse(aJsonString);
    proofOfWorkNonceSize = json.proof_of_work_nonce_size;
    nonceLength = json.nonce_length;
    maxRevelationsPerBlock = json.max_revelations_per_block;
    maxOperationDataLength = json.maxe_operation_data_length;
    preservedCycles = json.preserved_cycles;
    blocksPerCycle = json.blocks_per_cycle;
    blocksPerCommitment = json.blocks_per_commitment;
    blocksPerRollSnapshot = json.blocks_per_roll_snapshot;
    blocksPerVotingPeriod = json.blocks_per_voting_period;
    timeBetweenBlocks = json.time_between_blocks;
    endorsersPerBlock = json.endorsers_per_blocks;
    hardGasLimitPerOperation = json.hard_gas_limit_per_operation;
    hardGasLimitPerBlock = json.hard_gas_limit_per_block;
    proofOfWorkThreshold = json.proof_of_work_threshold;
    tokensPerRoll = json.tokens_per_roll;
    michelsonMaximumTypevarSize = 
      json.michelson_maximum_typevar_size;
    seedNonceRevelationTip = 
      json.seed_nonce_revelation_tip;
    originationBurn = json.origination_burn;
    blockSecurityDeposit = json.block_security_deposit;
    endorsementSecurityDeposit = json.endorsement_security_deposit;
    blockReward = json.block_reward;
    endorsementReward = json.endorsement_reward;
    costPerByte = json.cost_per_byte;
    hardStorageLimitPerOperation = 
      json.hard_storage_limit_per_operation;
  }
  
  function toJson () : String {
    return haxe.Json.stringify(toDynamic());
  }
  function toDynamic () : Dynamic {
    var result : Dynamic = 
      { 
        proof_of_work_nonce_size : proofOfWorkNonceSize,
        nonce_length : nonceLength,
        max_revelations_per_block : maxRevelationsPerBlock,
        max_operation_data_length : maxOperationDataLength,
        preserved_cycles : preservedCycles,
        blocks_per_cycle : blocksPerCycle,
        blocks_per_commitment : blocksPerCommitment,
        blocks_per_roll_snapshot : blocksPerRollSnapshot,
        blocks_per_voting_period : blocksPerVotingPeriod,
        time_between_blocks : timeBetweenBlocks,
        endersors_per_block : endorsersPerBlock,
        hard_gas_limit_per_operation : hardGasLimitPerOperation,
        hard_gas_limit_per_block : hardGasLimitPerBlock,
        proof_of_work_threshold : proofOfWorkThreshold,
        tokens_per_roll : tokensPerRoll,
        michelson_maximum_typevar_size : michelsonMaximumTypevarSize,
        seed_nonce_revelationTip : seedNonceRevelationTip,
        origination_burn : originationBurn,
        block_security_deposit : blockSecurityDeposit,
        endorsement_security_deposit : endorsementSecurityDeposit,
        block_reward : blockReward,
        endorsement_reward : endorsementReward,
        cost_per_byte : costPerByte,
        hard_storage_limit_per_operation : hardStorageLimitPerOperation
      };
    return result;
  }
}

/*
{  "hard_gas_limit_per_block": $bignum,
    "proof_of_work_threshold": $int64,
    "tokens_per_roll": $mutez,
    "michelson_maximum_typevar_size": integer ∈ [0, 2^16-1],
    "seed_nonce_revelation_tip": $mutez,
    "origination_burn": $mutez,
    "block_security_deposit": $mutez,
    "endorsement_security_deposit": $mutez,
    "block_reward": $mutez,
    "endorsement_reward": $mutez,
    "cost_per_byte": $mutez,
    "hard_storage_limit_per_operation": $bignum }
*/
