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

import haxe.io.Bytes;
import realitz.tezos.rpc.Types;
import realitz.tezos.rpc.MetadataTypes;
import haxe.io.Bytes;
import realitz.tezos.rpc.Michelson;

class Delegate {
  var settable (default, null) : Bool;
  var value (default, null) : PublicKeyHash;

}

class Contract {
  var manager (default, null) : PublicKeyHash;
  var balance (default, null) : Mutez;
  var spendable (default, null)  : Bool;
  var delegate (default, null) : Delegate;
  var script (default, null) : ScriptedContract;
  var counter (default, null) : PositiveBigNum;
}

class ScriptedContract {
  var code (default, null) : MichelsonV1Expression; 
  var storage (default, null) : MichelsonV1Expression;
}
enum Operation {
  ContentsAndResults(protocol : Protocol, 
    chainId : ChainId, 
    hash : OperationHash, 
    branch : BlockHash, 
    contents : List<ContentAndResult>, 
    signature : Signature);
  Contents (protocol : Protocol, 
    chainId : ChainId, hash : OperationHash,
    branch : BlockHash, contents : List<Content>);
}

enum BallotType {
  Yay; 
  Nay; 
  Pass;
}

enum Content {
  Endorsement (level : Int);
  SeedNonceRevelation (level : Int, nonce  : Bytes);
  DoubleEndorsementEvidence (op1 : InlinedEndorsement, op2 : InlinedEndorsement);
  DoubleBakingEvidence (bh1 : BlockHeader, bh2 : BlockHeader);
  ActivateAccount (pkh : PublicKeyHash, secret : Bytes);
  Proposals (source : PublicKeyHash, period : Int, proposals : List <ProtocolHash>);
  Ballot (source : PublicKeyHash, period : Int, proposal : ProtocolHash, ballotType : BallotType);
  Reveal (source : Contract, fee : Mutez, counter : PositiveBigNum, 
          storageLimit : PositiveBigNum, publicKey : Signature
        );
  Transaction (source : Contract, fee : Mutez, counter : PositiveBigNum,
    gasLimit : PositiveBigNum, 
    storageLimit : PositiveBigNum, 
    amount : Mutez, 
    destination : Contract,
    parameters : MichelsonV1Expression);
  Origination (source : Contract, fee : Mutez, counter : PositiveBigNum, 
    gasLimit : PositiveBigNum,
    storageLimit : PositiveBigNum, 
    managerPubKey : PublicKeyHash,
    balance : Mutez,
    spendable : Bool,
    delegatable : Bool, 
    delegate : PublicKeyHash, 
    script : ScriptedContract
    );
    Delegation(source : Contract, fee : Mutez, counter : PositiveBigNum, 
      gasLimit : PositiveBigNum, 
      storageLimit : PositiveBigNum,
      delegate : PublicKeyHash
      );


}

class EndorsementMetadata {
  var balanceUpdates : List<BalanceUpdate>;
  var delegate : PublicKeyHash;
  var slot : List<Int>;
}
enum ContentAndResult {
  Transaction(source : Contract, nonce : Int, amount : Mutez, destination : Contract, 
    parameters : MichelsonV1Expression, result : OperationResult);
  Origination (source : ContractId, nonce : Int, managerPubKey : PublicKeyHash, 
    balance : Mutez, spendable : Bool, 
    delegatable : Bool, delegate : PublicKeyHash, 
    script : ScriptedContract, result : OperationResult);
  Delegation (source : Contract, nonce : Int, delegate : PublicKeyHash, 
    result :  OperationResult);
  Endorsement (level : Int, metadata : EndorsementMetadata);
  SeedNonceRevelation (level : Int, nonce : Bytes, metadata : List<BalanceUpdate>);
  DoubleEndorsementEvidence (op1 : InlinedEndorsement, op2 : InlinedEndorsement, metadata : List<BalanceUpdate>);
  DoubleBakingEvidence (bh1 : BlockHeader, bh2 : BlockHeader, metadata : List<BalanceUpdate>);
  ActivateAccount (pkh : PublicKeyHash, secret : Bytes, metadata : List<BalanceUpdate>);
  Proposals (source : PublicKeyHash, period : Int, proposals : List <ProtocolHash>);
  Ballot (source : PublicKeyHash, period : Int, proposal : ProtocolHash, ballot : BallotType);
  Reveal (source : ContractId, fee : Mutez, counter : PositiveBigNum, 
    gasLimit : PositiveBigNum, storageLimit : PositiveBigNum, 
    publicKey : PublicKeyHash, 
    metadata : RevealMetadata);
}


class RevealMetadata {
  var balanceUpdates (default, null) : List<BalanceUpdate>;
  var reveal (default, null) : OperationResult;
  var internalOperationResults (default, null) : List<OperationResult>;
}
  
enum OperationResultDelegationEnum {
  Applied (status : String);
  Failed (status : String, errors : List<Error>);
  Skipped (status : String);
  Backtracked (status : String, errors : List<Error>);    
}

enum OperationResultOriginationEnum {
  Applied(balanceUpdates : List<BalanceUpdate>, 
    originatedContracts : List<ContractId>, consumedGas : BigNum, storageSize : BigNum, 
    paidStorageSizeDiff : BigNum);
  Failed (status : String, errors : List<Error>); 
  Backtracked (status : String, 
    errors : List<Error>, 
    balanceUpdates : List<BalanceUpdate>, 
    originatedContracts : List<ContractId>,  
    consumedGas : BigNum, 
    storageSize : BigNum, 
    paidStorageSizeDiff : BigNum
    );
}

enum OperationResultRevealEnum {
  Applied (status : String);
  Failed (status : String, errors : List<Error>);
  Backtracked (status : String, errors : List<Error>);
}

enum OperationResultTransactionEnum {
  Applied (status : String, storage : MichelsonV1Expression, 
    balanceUpdates : List<BalanceUpdate>, 
    originatedContracts : List<ContractId>,
    consumedGas : BigNum, storageSize : BigNum, 
    paidStorageSizeDiff : BigNum);
  Failed (status : String, errors : List<Error>); 
  Skipped (status : String); 
  Backtracked (status : String, errors : List<Error>, 
    storage : MichelsonV1Expression, 
    balanceUpdates : List<BalanceUpdate>, 
    originatedContracts : List<ContractId>, 
    consumedGas : BigNum, 
    storageSize : BigNum, 
    paidStorageSizeDiff : BigNum);
}


//TODO: This design is not what the core code reflects. 
//We need some kind of a generic so that the type could be 
//varying with the type of the result. For example,
//If the Result is a transaction -> return OperationResultTransactionEnum.
//As it stands, this enum doesn't reflect that.
enum OperationResultEnum {
  OperationResultTransactionEnum;
  OperationResultRevealEnum;
  OperationResultOriginationEnum;
  OperationResultDelegationEnum;
}

class OperationResultMetadata {
  var balanceUpdates (default, null) : List<BalanceUpdate>;
  var operationResult (default,  null) : OperationResultEnum;
  var internalOperationResults (default, null) : List<OperationResult>;
}

enum OperationResult {
  Reveal (source : ContractId, fee : Mutez, 
      counter : PositiveBigNum, 
      gasLimit : PositiveBigNum,
      storageLimit : PositiveBigNum, 
      publicKey : PublicKey, 
      metadata : OperationResultMetadata);
  Transaction (source : ContractId, fee : Mutez, 
    counter : PositiveBigNum, 
    gasLimit : PositiveBigNum, 
    storageLimit : PositiveBigNum, 
    amount : Mutez,
    destination : ContractId, 
    parameters : MichelsonV1Expression, 
    metadata : OperationResultMetadata
    );
  Origination (source : ContractId, fee : Mutez, 
    counter : PositiveBigNum, 
    gasLimit : PositiveBigNum, 
    storageLimit : PositiveBigNum,
    managerPubKey : PublicKeyHash, 
    balance : Mutez, 
    spendable : Bool,
    delegatable : Bool, 
    delegate : PublicKeyHash, 
    script : ScriptedContract,
    metadata : OperationResultMetadata
    );
    Delegation(
      source : ContractId, fee : Mutez, 
      counter : PositiveBigNum, 
      gasLimit : PositiveBigNum, 
      storageLimit : PositiveBigNum,
      delegate : PublicKeyHash, 
      metadata : OperationResultMetadata
    );
}

/*

  $operation.alpha.internal_operation_result:
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
  $positive_bignum:
    string
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
    string
*/