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
