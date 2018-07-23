{-|
Module      : Realitz.Tezos.RPC.Types
Description : The RPC types needed to interact with Tezos node.
Copyright   : (c) 
License     : GPL-3
Maintainer  : dinkar.ganti@gmail.com
Stability   : 
Portability : 
-}

module Realitz.Tezos.RPC.Types where

import Data.Text 
import Data.Time
import Data.Word (Word8)
import Realitz.Tezos.RPC.Protocol
import Data.ByteString (ByteString)
import Realitz.Tezos.RPC.Error as Error
import Realitz.Tezos.RPC.Operations.ResultStatus
import Realitz.Tezos.RPC.Operations.InternalResultStatus
import Realitz.Tezos.RPC.Common


{-| 
-- @ 

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

-- @ 
-}

data TestChainStatus = 
  NotRunning
  | Forking (Protocol, Expiration)
  | Running (ChainId, BlockHash, Protocol, Expiration)
  deriving(Show)

data InjectBlock = 
  InjectBlock {
    blockData :: String
  }

{-| 
  Shell header
-}
{-
type shell_header = {
  level: Int32.t ;
  proto_level: int ; (* uint8 *)
  predecessor: Block_hash.t ;
  timestamp: Time.t ;
  validation_passes: int ; (* uint8 *)
  operations_hash: Operation_list_list_hash.t ;
  fitness: Fitness.t ;
  context: Context_hash.t ;
}
-}
data ShellHeader = ShellHeader {
  level :: Int 
  , proto_level :: Word8
  , predecessor :: BlockHash
  -- | The timestamp for the header. 
  -- | Currently we are using simply 'UTCTime'
  -- | Should we change it to 'UTC' to account for 
  -- | leapseconds?
  , timestamp :: UTCTime
  , validationPasses :: Word8
  , operationsHash :: [OperationHash]
  , fitness :: Fitness 
  , context :: ContextHash
}

{-|
-- Copied from the 'fitness.ml'
The fitness, or score, of a block, that allow the Tezos to
decide which chain is the best. A fitness value is a list of 
byte sequences. They are compared as follows: shortest lists
are smaller; lists of the same length are compared according to
the lexicographical order.

-}
newtype Fitness =
  Fitness {_unByteSeqL :: [ByteString]} deriving (Show)


{-| 
-- @
  type block_metadata = {
    protocol_data: Proto.block_header_metadata ;
    test_chain_status: Test_chain_status.t ;
    (* for the next block: *)
    max_operations_ttl: int ;
    max_operation_data_length: int ;
    max_block_header_length: int ;
    operation_list_quota: operation_list_quota list ;
  }
-- @
-}

data  BlockMetadata = BlockMetadata {
  protocolData :: Protocol 
  , testChainStatus :: TestChainStatus 
  , maxOperationsTTL :: Int 
  , maxOperationDataLength :: Int
  , operationListQuota :: OperationListQuota
} deriving(Show)

{-| 

type operation_list_quota = {
  max_size: int ;
  max_op: int option ;
}
-}
data OperationListQuota = OperationListQuota {
  maxSize :: Int 
  , maxOp :: Maybe Int
} deriving(Show)

{-| 
-- @ 
  -- the scala code. Need to get to the original ml file that has this 
  -- information.
  case class OperationMetadata(
                              delegate: Option[String],
                              slots: Option[List[Int]],
                              balanceUpdates: Option[List[AppliedOperationBalanceUpdates]],
                              operationResult: Option[AppliedOperationResultStatus],
                              internalOperationResult: Option[AppliedInternalOperationResult]
                              )
-- @
-}

data OperationMetadata = OperationMetadata {
  delegate :: Maybe Delegate
  , slots :: [Int]
  , balanceUpdates :: [BalanceUpdate]
  , operationResult :: [ResultStatus]
  , internalOperationResult :: [InternalResultStatus]
} deriving (Show)

