module Realitz.Tezos.RPC.Operations.Operation where

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

data OperationGroup = Operation {
  protocol :: Protocol 
  , chainId :: Maybe Chain 
  , hash :: BlockHash 
  , branch :: Branch 
  , contents :: [Operation]
  , signature :: Maybe Signature
}
