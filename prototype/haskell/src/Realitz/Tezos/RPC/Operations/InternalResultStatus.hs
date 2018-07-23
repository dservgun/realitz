module Realitz.Tezos.RPC.Operations.InternalResultStatus where 
import Realitz.Tezos.RPC.Operations.ResultStatus
import Realitz.Tezos.RPC.Common

{-| 
-- @
  case class AppliedInternalOperationResult(
                                           kind: String,
                                           source: String,
                                           nonce: Int,
                                           publicKey: Option[String],
                                           result: AppliedOperationResultStatus,
                                           amount: Option[Int],
                                           destination: Option[String],
                                           parameters: Option[Any],
                                           managerPubKey: Option[String],
                                           balance: Option[Int],
                                           spendable: Option[Boolean],
                                           delegatable: Option[Boolean],
                                           delegate: Option[String],
                                           script: Option[ScriptedContracts],
                                           )

--@ 
-}

data InternalResultStatus = InternalResultStatus {
    kind :: UpdateKind
    , source :: TzAddress
    , nonce :: Int
    , publicKey :: Maybe PublicKey 
    , result :: ResultStatus
    , amount :: Maybe Int 
    , destination :: Maybe TzAddress 
    , parameters :: Maybe Parameter
    , managerPubKey :: Maybe PublicKey 
    , balance :: Maybe Int 
    , spendable :: Maybe Bool 
    , delegatable :: Maybe Bool 
    , delagate :: Maybe TzAddress 
    , script :: Maybe ScriptedContract
  } deriving (Show)