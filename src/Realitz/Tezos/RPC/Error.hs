module Realitz.Tezos.RPC.Error where 

import Data.Text
newtype BlockError = BlockError{_unError :: Text} deriving (Show)

