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

class ContextConstants {
  var blockReward (default, null) : Mutez;
  var blockSecurityDeposit (default, null) : Mutez;
  var blocksPerCommitment (default, null) : Int;
  var blocksPerCycle (default, null) : Int;
  var blocksPerRollSnapshot (default, null) : Int;
  var blocksPerVotingPeriod (default, null) : Int;
  var costPerByte (default, null) : Mutez;
  var endorsementReward (default, null) : Mutez;
  var endorsementSecurityDeposit (default, null) : Mutez;
  var endorsersPerBlock (default, null) : Int;
  var hardGasLimitPerBlock (default, null) : BigNum;
  var hardGasLimitPerOperation (default, null) : BigNum;
  var hardStorageLimitPerOperation (default, null) : BigNum;
  var maxOperationDataLength (default, null) : Int;
  var maxRevelationsPerBlock (default, null) : Int;
  var michelsonMaximumTypevarSize (default, null) : Int;
  var nonceLength (default, null) : Int;
  var originationBurn (default, null) : Mutez;
  var preservedCycles (default, null) : Int;
  var proofOfWorkNonceSize (default, null) : Int;
  var proofOfWorkThreshold (default, null) : Int64;
  var seedNonceRevelationTip (default, null) : Mutez;
  var timeBetweenBlocks (default, null) : List<Int64>;
  var tokensPerRoll (default, null) : Mutez;
  public function new (json : Dynamic) {
    blockReward = json.block_reward;
    blockSecurityDeposit = json.block_security_deposit;
    blocksPerCommitment = json.blocks_per_commitment;
    blocksPerCycle = json.blocks_per_cycle;
    blocksPerRollSnapshot = json.blocks_per_roll_snapshot;
    blocksPerVotingPeriod = json.blocks_per_voting_period;
    costPerByte = json.cost_per_byte;
    endorsementReward = json.endorsement_reward;
    endorsementSecurityDeposit = json.endorsement_security_deposit;
    endorsersPerBlock = json.endorsers_per_blocks;
    hardGasLimitPerBlock = json.hard_gas_limit_per_block;
    hardGasLimitPerOperation = json.hard_gas_limit_per_operation;
    hardStorageLimitPerOperation = json.hard_storage_limit_per_operation;
    maxOperationDataLength = json.maxe_operation_data_length;
    maxRevelationsPerBlock = json.max_revelations_per_block;
    michelsonMaximumTypevarSize = json.michelson_maximum_typevar_size;
    nonceLength = json.nonce_length;
    originationBurn = json.origination_burn;
    preservedCycles = json.preserved_cycles;
    proofOfWorkNonceSize = json.proof_of_work_nonce_size;
    proofOfWorkThreshold = json.proof_of_work_threshold;
    seedNonceRevelationTip = json.seed_nonce_revelation_tip;
    timeBetweenBlocks = json.time_between_blocks;
    tokensPerRoll = json.tokens_per_roll;
  }

  function toJson () : String {
    return haxe.Json.stringify(toDynamic());
  }
  function toDynamic () : Dynamic {
    var result : Dynamic = 
      { 
        block_reward : blockReward,
        block_security_deposit : blockSecurityDeposit,
        blocks_per_commitment : blocksPerCommitment,
        blocks_per_cycle : blocksPerCycle,
        blocks_per_roll_snapshot : blocksPerRollSnapshot,
        blocks_per_voting_period : blocksPerVotingPeriod,
        cost_per_byte : costPerByte,
        endersors_per_block : endorsersPerBlock,
        endorsement_reward : endorsementReward,
        endorsement_security_deposit : endorsementSecurityDeposit,
        hard_gas_limit_per_block : hardGasLimitPerBlock,
        hard_gas_limit_per_operation : hardGasLimitPerOperation,
        hard_storage_limit_per_operation : hardStorageLimitPerOperation,
        max_operation_data_length : maxOperationDataLength,
        max_revelations_per_block : maxRevelationsPerBlock,
        michelson_maximum_typevar_size : michelsonMaximumTypevarSize,
        nonce_length : nonceLength,
        origination_burn : originationBurn,
        preserved_cycles : preservedCycles,
        proof_of_work_nonce_size : proofOfWorkNonceSize,
        proof_of_work_threshold : proofOfWorkThreshold,
        seed_nonce_revelationTip : seedNonceRevelationTip,
        time_between_blocks : timeBetweenBlocks,
        tokens_per_roll : tokensPerRoll
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
