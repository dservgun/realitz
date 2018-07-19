module Model.Core.Technician where 

import Model.Core.Lab 
import Data.Time
import Model.Core.Types

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
