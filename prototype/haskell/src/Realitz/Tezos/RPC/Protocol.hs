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