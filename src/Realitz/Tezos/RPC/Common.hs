module Realitz.Tezos.RPC.Common where 

import Data.ByteString as ByteString
import Data.Text

newtype Storage = Storage {_unstorage :: ByteString}
instance Show Storage where 
  show (Storage aByteArray) = show $ "Storage " ++ 
    (show $ ByteString.take 80 aByteArray)
newtype Contract = Contract {_uncontract :: Text} deriving (Show)

newtype TzAddress = TzAddress {_tzAddress :: Text} deriving (Show)
newtype PublicKey = PublicKey {_pubKey :: Text} deriving (Show)
newtype Parameter = Parameter {_param :: Text} deriving (Show) 

{-
  case class ScriptedContracts(
                              storage: Any,
                              code: Any
                              )

  case class InlinedEndorsement(
                               branch: String,
                               operation: InlinedEndorsementContents,
                               signature: Option[String]
                               )

  case class InlinedEndorsementContents(
                                       kind: String,
                                       block: String,
                                       level: String,
                                       slots: List[Int]
                                       )
-}

data ScriptedContract = 
  ScriptedContract {
    storage :: ByteString
    , code :: ByteString
  } deriving (Show)
