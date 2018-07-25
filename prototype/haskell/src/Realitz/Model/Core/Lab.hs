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
Module      : Realitz.Model.Core.Lab
Description : Lab administration information.
Copyright   : (c) 
License     : GPL-3
Maintainer  : dinkar.ganti@gmail.com
Stability   : 
Portability : 
-}

module Realitz.Model.Core.Lab 
  (
    Lab(..)
    , validLicense
  )
where
import Control.Monad.IO.Class
import Data.Time 
import Realitz.Model.Core.Types

data Lab = Lab {
  _contact :: ContactInformation
  , _licenseStartDate :: UTCTime
  , _licenseEndDate :: UTCTime
} deriving (Show)


{-| A license is valid for a lab if the date on which this function is invoked, is within
   the 'licenseStartDate' and 'licenseEndDate'. Also, this implies, that on a block chain,
   the current timestamp is the same as the the contract is executed.
-}
-- The operation is deliberatly inside 'MonadIO' because the current date is not available outside 
-- an IO monad. Obviously, what feels elegant and obvious now, may not be in hindsight.
validLicense :: MonadIO m => Lab -> m Bool 
validLicense = undefined

