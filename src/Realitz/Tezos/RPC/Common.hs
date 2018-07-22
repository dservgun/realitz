module Realitz.Tezos.RPC.Common where 

import Data.ByteString as ByteString
import Data.Text
import Data.Time


newtype Block = Block {_unblock :: Text} deriving (Show)
newtype BlockHash = BlockHash {_unBlockHash :: ByteString} deriving (Show)
newtype BlockLevel = BlockLevel {_unBlockLevel :: ByteString} deriving (Show)
newtype ChainId = ChainId {_unChainId :: Text} deriving (Show)
newtype ContextHash = ContextHash {_unContextHash :: ByteString} deriving (Show)
newtype Contract = Contract {_uncontract :: Text} deriving (Show)
newtype Delegate = Delegate {_unDelegate :: ByteString} deriving (Show)
newtype Expiration = Expiration {_unExpiration :: UTCTime} deriving (Show)
newtype InvalidBlocks = InvalidBlocks {_invBlocks :: [BlockHash]} deriving (Show)
newtype OperationHash = OperationHash {_unOpHash :: ByteString} deriving (Show) 
newtype Parameter = Parameter {_param :: Text} deriving (Show) 
newtype PublicKey = PublicKey {_pubKey :: Text} deriving (Show)
newtype Signature = Signature {_unSignature :: ByteString} deriving (Show)
newtype Storage = Storage {_unstorage :: ByteString}
newtype TzAddress = TzAddress {_tzAddress :: Text} deriving (Show)
newtype UpdateKind = UpdateKind {_updateKind :: Text} deriving (Show)
newtype ValidBlocks = ValidBlocks {_validBlocks :: [BlockHash]} deriving (Show)

instance Show Storage where 
  show (Storage aByteArray) = show $ "Storage " ++ 
    (show $ ByteString.take 80 aByteArray)

{-


-}

{-| 
-- @
  case class ScriptedContracts(
                              storage: Any,
                              code: Any
                              )
-- @
-}
data ScriptedContract = 
  ScriptedContract {
    storage :: ByteString
    , code :: ByteString
  } deriving (Show)

{-| 
-- @
  case class InlinedEndorsement(
                               branch: String,
                               operation: InlinedEndorsementContents,
                               signature: Option[String]
                               )
-- @
-}

data InlinedEndorsement = InlinedEndorsement {
  branch :: Text
  , operation :: InlinedEndorsementContent
  , signature :: Maybe Signature
} deriving Show

{-| 
-- @

  case class InlinedEndorsementContents(
                                       kind: String,
                                       block: String,
                                       level: String,
                                       slots: List[Int]
                                       )
-- @
-}

data InlinedEndorsementContent = InlinedEndorsementContent {
  endorsementKind :: Text
  , block :: BlockHash
  , level :: BlockLevel
  , slots :: [Int]
} deriving Show
