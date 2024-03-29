--***************************************************************************--
--                                                                           --
-- Open Source License                                                       --
-- Copyright (c) Dinkar Ganti, dinkar.ganti@gmail.com                        --
--                                                                           --
-- Permission is hereby granted, free of charge, to any person obtaining a   --
-- copy of this software and associated documentation files (the "Software"),--
-- to deal in the Software without restriction, including without limitation --
-- the rights to use, copy, modify, merge, publish, distribute, sublicense,  --
-- and/or sell copies of the Software, and to permit persons to whom the     --
-- Software is furnished to do so, subject to the following conditions:      --
--                                                                           --
-- The above copyright notice and this permission notice shall be included   --
-- in all copies or substantial portions of the Software.                    --
--                                                                           --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR--
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  --
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   --
-- THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER--
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   --
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       --
-- DEALINGS IN THE SOFTWARE.                                                 --
--                                                                           --
--***************************************************************************--

module Realitz.Tezos.RPC.Operations.Operation where

import Data.ByteString
import Data.Text
import Realitz.Tezos.RPC.Common
import Realitz.Tezos.RPC.Protocol

newtype OperationKind = OperationKind {_unOperation :: Text} deriving Show 
{-| 
-- @
  case class Operation(
                       kind: String,
                       block: Option[String],
                       level: Option[Int],
                       slots: Option[List[Int]],
                       nonce: Option[String],
                       op1: Option[InlinedEndorsement],
                       op2: Option[InlinedEndorsement],
                       bh1: Option[BlockHeader],
                       bh2: Option[BlockHeader],
                       pkh: Option[String],
                       secret: Option[String],
                       proposals: Option[List[String]],
                       period: Option[String],
                       source: Option[String],
                       proposal: Option[String],
                       ballot: Option[String],
                       fee: Option[String],
                       counter: Option[Int],
                       gasLimit: Option[String],
                       storageLimit: Option[String],
                       publicKey: Option[String],
                       amount: Option[String],
                       destination: Option[String],
                       parameters: Option[Any],
                       managerPubKey: Option[String],
                       balance: Option[String],
                       spendable: Option[Boolean],
                       delegatable: Option[Boolean],
                       delegate: Option[String]
                       )

-- @
-}

data Operation = Operation {
  _kind :: OperationKind
  , block :: Maybe BlockHash 
  , level :: Maybe Level
  , slots :: [Slot]
  , nonce :: Maybe Nonce 
  , op1 :: Maybe InlinedEndorsement
  , op2 :: Maybe InlinedEndorsement
  , bh1 :: Maybe BlockHeader 
  , bh2 :: Maybe BlockHeader
  , pkh :: Maybe ByteString
  , secret :: Maybe ByteString
  , proposals :: [ByteString] 
  , period :: Maybe ByteString
  , source :: Maybe ByteString 
  , ballot :: Maybe ByteString 
  , fee :: Maybe ByteString
  , counter :: Maybe Int
  , gasLimit :: Maybe String 
  , storageLimit :: Maybe String 
  , publicKey :: Maybe PublicKey
  , amount :: Maybe String
  , destination :: Maybe TzAddress
  , parameters :: Maybe Parameter 
  , managerPubKey :: Maybe PublicKey 
  , balance :: Maybe ByteString 
  , spendable :: Maybe ByteString 
  , delegatable :: Maybe Bool 
  , delegate :: Maybe TzAddress
} deriving Show


{-| 
-- @
  case class OperationGroup (
                              protocol: String,
                              chain_id: Option[String],
                              hash: String,
                              branch: String,
                              contents: Option[List[Operation]],
                              signature: Option[String],
                            )
-- @
-}

data OperationGroup = OperationGroup {
  protocol :: Protocol 
  , chainId :: Maybe ChainId 
  , hash :: BlockHash 
  , branch :: Branch 
  , contents :: [Operation]
  , signature :: Maybe Signature
}
