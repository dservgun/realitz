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
  var testChainStatus : TestChainStatus
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
  var nonceHash : BlockHash 
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

class BalanceUpdate {
  
}