module Realitz.Tezos.RPC.Operations.ResultStatus where 

import Realitz.Tezos.RPC.Error as Error
import Realitz.Tezos.RPC.Common
import Data.Text
newtype UpdateKind = UpdateKind {_updateKind :: Text} deriving Show 

{-| 
-- @
  case class AppliedOperationBalanceUpdates(
                                             kind: String,
                                             contract: Option[String],
                                             change: Int,
                                             category: Option[String],
                                             delegate: Option[String],
                                             level: Option[Int]
                                           )

-- @
-}

data BalanceUpdate = BalanceUpdate {
  kind :: UpdateKind
  , contract :: Maybe Contract 
  , change :: Int 
  , category :: Error.Category
} deriving (Show)

{-| 
-- @
  case class AppliedOperationResultStatus(
                                   status: String,
                                   errors: Option[List[String]],
                                   storage: Option[Any],
                                   balanceUpdates: Option[AppliedOperationBalanceUpdates],
                                   originatedContracts: Option[String],
                                   consumedGas: Option[Int],
                                   storageSizeDiff: Option[Int]
                                   )

-- @
-}
data ResultStatus = ResultStatus {
  status :: String 
  , errors :: [Error.BlockError]  
  , storage :: Maybe Storage 
  , balanceUpdate :: Maybe BalanceUpdate 
  , originatedContracts :: Maybe Contract 
  , consumedGas :: Maybe Int 
  , storageSizeDiff :: Maybe Int
} deriving (Show)

