{-|
Module      : Model.Core.Report
Description : This module defines the reports, associated contamination limits.
Copyright   : (c) 
License     : GPL-3
Maintainer  : dinkar.ganti@gmail.com
Stability   : 
Portability : 
-}

module Realitz.Tezos.RPC.Protocol where
import Data.Text
newtype Protocol = Protocol {_unP :: Text} deriving (Show)


data Encoding = Base58E | Base64E deriving (Show)
data Decoding = Base58D | Base64D deriving (Show)

{-| Base58Encoding of the 'Protocol'.
-}
encode :: Encoding -> Protocol -> Text
encode = undefined

{-| Decode a Base58Encoded byte array.
-}
decode :: Decoding -> Text -> Protocol
decode = undefined 