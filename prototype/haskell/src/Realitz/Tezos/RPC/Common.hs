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

module Realitz.Tezos.RPC.Common where 

import Data.ByteString as ByteString
import Data.Text
import Data.Time


newtype Block = Block {_unblock :: Text} deriving (Show)
newtype BlockHash = BlockHash {_unBlockHash :: ByteString} deriving (Show)
newtype BlockLevel = BlockLevel {_unBlockLevel :: ByteString} deriving (Show)
newtype BlockHeader = BlockHeader {_unBlockHeader :: ByteString} deriving(Show)
newtype Nonce = Nonce {_unNonce :: Int} deriving (Show)
newtype Branch = Branch {_unBranch :: ByteString} deriving (Show)
newtype ChainId = ChainId {_unChainId :: Text} deriving (Show)
newtype ContextHash = ContextHash {_unContextHash :: ByteString} deriving (Show)
newtype Contract = Contract {_uncontract :: Text} deriving (Show)
newtype Delegate = Delegate {_unDelegate :: ByteString} deriving (Show)
newtype Expiration = Expiration {_unExpiration :: UTCTime} deriving (Show)
newtype InvalidBlocks = InvalidBlocks {_invBlocks :: [BlockHash]} deriving (Show)
newtype Level = Level {_unLevel :: Int} deriving (Show)
newtype Slot = Slot {_unSlot :: Int} deriving (Show)
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
