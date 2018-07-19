module Model.Core.Types where 

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
