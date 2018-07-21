{-|
Module      : Realitz.Model.Core.Technician
Description : Technician information.
Copyright   : (c) 
License     : GPL-3
Maintainer  : dinkar.ganti@gmail.com
Stability   : 
Portability : 
-}

module Realitz.Model.Core.Technician where 

import Realitz.Model.Core.Lab 
import Data.Time
import Realitz.Model.Core.Types


{-| The technician performing the test for the associated Lab.
-}
data Technician = Technician {
  _technicianAddress :: TezosAddress
  , _contact :: ContactInformation
  , _lab :: Lab
  , _licenseStartDate :: UTCTime
  , _licenseEndDate :: UTCTime
} deriving (Show)


{-| The '_licenseEndDate' for the 'Lab' and the 'Technician' should 
be greater than the date of the eecution of the method.
-}
licenseValidForTechnician :: Technician -> IO Bool 
licenseValidForTechnician = undefined
