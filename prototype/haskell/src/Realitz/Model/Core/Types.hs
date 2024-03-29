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
Module      : Realitz.Model.Core.Types
Description : Commonly used types.
Copyright   : (c) 
License     : GPL-3
Maintainer  : dinkar.ganti@gmail.com
Stability   : 
Portability : 
-}

module Realitz.Model.Core.Types where 

-- Notes: How do we decide if a type is within a module as in the case
-- here, or should a type be promoted to a module. Current rule of thumb
-- is that if the same/similar record accessor shows up, it is probably time
-- to split the module into respective types. Some types are more than that
-- for example 'Error' or 'Metadata'; these types are obviously quite often used
-- in mulitple scenarios, therefore having require a module of their own. 


import Data.Text 
import Control.Monad.IO.Class
import Network.URL


newtype CasNumber = CasNumber {_elements :: [Int]} deriving (Show)
newtype TezosAddress = TezosAddress {_add :: Text} deriving (Show)

{-|
Contamination of a compound is measured in ratio and this is conventional
depending on the nature of the contaminant. Here we are representing the
contamination by the most commonly used measures in most reports, though
the units can be better expressed as more contaminants are monitored.
-}
data ContaminationUOM = 
    PartsPerMillion | PartsPerBillion deriving (Show)

{-|
  The amount of '_cas' in terms of the 'ContaminationUOM'. For example, 
  200 'PartsPerBillion' of _Tetrachloroethene (PCE)_. The final report needed to
  obtain an NFA on a property will have 2 parameters, the actual contamination 
  measured and the allowed limit of the contamination. Obviously, the 
  measured value needs to be less than the allowed limit to obtain closure.
-}

data Contamination = Contamination {
  _cas :: CasNumber
  , _amount :: Integer
  , _unitOfMeasure :: ContaminationUOM
} deriving (Show)


data Email = Email {_unEmail :: Text} deriving (Show) 
data Phone = Phone {_unPhone :: Text} deriving (Show) 
data Address = Address{_unAddress :: Text} deriving (Show)

data ContactInformation = ContactInformation {
  _contactDetails :: Address
  , _contactEmail :: Email 
  , _contactPhone :: Phone 
} deriving (Show)


{-| 
  Local environment regulatory body establishes a contamination protocol 
  that needs to be followed in publishing results of a test.
-}
data CotaminantTestProtocol = CotaminantTestProtocol {
  _protoUrl :: URL 
  , comments :: Text
} deriving (Show) 


{- 
  Given a 'CotaminantTestProtocol' return all protocols that can be considered as alternatives. 
  Returns an empty list or more generally a 'Functor' that has no elements 'equal' to the input 'CotaminantTestProtocol'.
  For example if there are not protocols other than the one input, we can safely say that this protocol 
  is unique. 
-}
equivalentProtocols :: (MonadIO m, Functor f)  => CotaminantTestProtocol -> m (f CotaminantTestProtocol)
equivalentProtocols = undefined 

-- | The case manager responsible for this property.
data CaseManager = CaseManager {
  _cmName :: Text
  , _contact :: TezosAddress
  , _phone :: Text 
  , _email :: Text
} deriving (Show)
