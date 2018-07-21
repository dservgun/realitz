module Realitz.Tezos.RPC.Error where 

import Data.Text
newtype BlockError = BlockError{_unError :: Text} deriving (Show)

{-| 
-- @
type error_category = [ `Branch | `Temporary | `Permanent ]
-- @
-}

data Category = Permanent | Temporary | Branching
  deriving (Show)