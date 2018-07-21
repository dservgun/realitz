
module Realitz.Tezos.RPC.InvalidBlock where 
import Realitz.Tezos.RPC.Types
import Realitz.Tezos.RPC.Error
import Data.Text

data InvalidBlock = 
  InvalidBlock {
  block :: BlockHash
  , level :: Int
  , errors :: [BlockError]
  } deriving (Show)


test :: (Functor f) => Text -> f Text -> Text
test = undefined