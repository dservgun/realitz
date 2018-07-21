{-|
Module      : Realitz.Tezos.RPC.Types
Description : The RPC types supported by Tezos Node.
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


newtype ChainId = ChainId {_unChainId :: Text} deriving (Show)

newtype BlockHash = BlockHash {_unBlockHash :: ByteString} deriving (Show)
newtype Expiration = Expiration {_unExpiration :: UTCTime} deriving (Show)
newtype OperationHash = OperationHash {_unOpHash :: ByteString} deriving (Show) 
newtype ContextHash = ContextHash {_unContextHash :: ByteString} deriving (Show)
newtype InvalidBlocks = InvalidBlocks {_invBlocks :: [BlockHash]} deriving (Show)
newtype ValidBlocks = ValidBlocks {_validBlocks :: [BlockHash]} deriving (Show)

data ChainState = 
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