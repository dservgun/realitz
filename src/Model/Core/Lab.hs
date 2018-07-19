module Model.Core.Lab 
  (
    Lab(..)
    , validLicense
  )
where
import Control.Monad.IO.Class
import Data.Time 
import Model.Core.Types

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

