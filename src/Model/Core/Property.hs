{-|
Module      : Property
Description : This module outlines the work flow for an entity cleaning up a 
              property contaminated with oil.
              Cleaning up of properties contaminated with oil present a significant
              challenge to any property owner. This module help maintain the steps 
              involved in cleaning up a property.
Copyright   : (c) 
License     : GPL-3
Maintainer  : dinkar.ganti@gmail.com
Stability   : 
Portability : 
-}
module Model.Core.Property where 

import Data.Text
import Data.Time
import Data.Set
{-|
  = Remediation of properties contaminated with oil.
  The process of remediation of contaminated properties has quite a few
  steps and does provide an investment opportunity to investors with an 
  appetite for risk. However, the transaction fees adds up and the risk 
  can be offset if most of the cash transactions are managed on a Tezos.

  == Request for investment phase.

  During this phase, the group short lists a set of properties that meet a particular risk profile. 

  == Research phase 

  During this phase all the relevant environment reports need to be collected for investors to vote on the project. This phase also includes a phase to send invites to an investor to solicit interest in the project. Initially, 
  this can be a simple mailing list and a voting phase with the quorum of investors needed for the property.

  == Offer phase 

  During this phase, an offer is made to the seller that usually includes an option to purchase the property at a price before an agreed upon date. The option has a premium as well as a list of tasks the option holder agrees to perform, which in this case involves cleaning up the property during before the option expires. Obviously, the profitability of the deal is influenced by a number of factors and this document will outline some of the parameters that need to be managed during the active phase of a project.

  == Pre-cleanup phase
  Option agreement also has a liability clause that governs the participation of a licensed contractor so that the property is covered. 

  == Permitting and approvals
  Before a property project can commence, the county needs to approve of the cleanup plan with some scheduling requirements. 

  == Cleanup process
  This is when the cleanup commences. As part of the cleanup the tanks are removed if any along with their certificate of removal with timestamps so that they can be traced at any time by any regulatory authority. 

  == Closure process
  Depending on the choice of available technologies, such as <https://shop.sarvabioremed.com/collections/vaporremed>, the timeline for cleanup may vary. However, the final cleanup requires that a contamination report in soil and associated groundwater report to ensure that the property is clean. 

  == DEP's NFA report
  If all goes well, the DEP issues an NFA report on the property. 

  == Option Exercise process
  A vote is setup to seek approval to exercise the option if the date of cleanup is within the option's expiration date. For the vote to be meaningful this process needs to present the investors with a potential price on the property as a result of the cleanup. The request to vote needs to be accompanied with the potential price so that the investor group can arrive at a consensus price. 

  == Settlement 
  Obviously this is what the group is waiting for and this can take months as the remediation projects usually have multiple phases,as we mention earlier, these projects present high risk to an individual investor, though can present some opportunity when the risk is shared among multiple investors.

  == Escrow account
  Settlement amount is transfered to an escrow account that holds the settlement amout for a settlement time (default to 24 hours). Distribution is executed using generally accepted distribution practices.
-}

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

data Lab = Lab {
  _contactDetails :: Address 
  , _contactEmail :: Email 
  , _contactPhone :: Phone
  , _licenseStartDate :: UTCTiime
  , _licenseEndDate :: UTCTime
} deriving (Show)

data ContaminationProtocol = ContaminationProtocol {
  _protoUrl :: URL 
  , comments :: Text
} deriving (Show) 


{- 
  Given a 'ContaminationProtocol' return all protocols that can be used instead. Will return an empty 
  list or more generally a 'Functor' that has no elements 'equal' to the input 'ContaminationProtocol'.
  For example if there are not protocols other than the one input, we can safely say that this protocol 
  is unique. 
-}
equivalentProtoocols :: (MonadIO m, Functor f)  => ContaminationProtocol -> m (f ContaminationProtocol)
equivalentProtocols = undefined 

{-| A license is valid for a lab if the date on which this function is invoked, is within
   the 'licenseStartDate' and 'licenseEndDate'. Also, this implies, that on a block chain,
   the current timestamp is the same as the the contract is executed.
-}
-- The operation is deliberatly inside 'MonadIO' because the current date is not available outside 
-- an IO monad. Obviously, what feels elegant and obvious now, may not be in hindsight.
validLicense :: MonadIO m => Lab -> m Boolean 
validLicense = undefined



data Detail = Detail {
  chemicalName :: Text
  , casNumber :: CasNumber
  , contamination :: Contamination
  , _measuredOn :: UTCTiime
  -- | The laboratory performing the tests.
  , _lab :: Lab
  -- | The specific protocol the laboratory is tasked to follow to 
  -- | measure contamination. Typically, these are in the forma
  -- | of a link to a pdf document published by either the state or 
  -- | a federal regulatory body. Smoe protocols could be sufficiently 
  -- | standard that an industry association could be responsible for 
  -- | pubishing the protocol.
  , _protocol :: ContaminationProtocol
} deriving (Show)

{-| 
  A summary report of contamination captures state of the contamination.
-}
data Summary = Summary {
  _summary :: Text
  , _details :: Text
} deriving (Show)

-- | Given a set of details, is there a trendline or has the data remained flat.
trendlineEstablished :: Functor f => f Detail -> Boolean 
trendlineEstablished = undefined

{-| The state the property is listed in, should be one of the 51 states in
the country. This version supports US states, though we can support variety of
  countries that need this application.
-}
data State = State {
  stateCode :: Text
  , stateDescription :: Text
} deriving (Show)


data Property = Property {
  _name :: Text
  , _address :: Text
  , _state :: State
} deriving (Show)

data ReportIdentifier = ReportIdentifier {_id :: Text} deriving (Show)
data ReportType = 
  RemedialInvestigation | RiskAssessment | Cleanup deriving (Show)

data Report = Report {
  -- | Date on which the report was prepared.
  preparedDate :: UTCTime 
  -- | The type of report. 
  , reportType :: ReportType
  -- | 'Detail' outlines additional detail based on the phase of the 
  -- | contaminated property. 
  , reportDetails :: Set Detail
  -- | A high level summary of the report. 
  , reportSummary :: Set Summary
  , caseNumber :: ReportIdentifier
  , preparer :: CaseManager
} deriving (Show)

-- | The case manager responsible for this property.
data CaseManager = CaseManager {
  _cmName :: Text
  , _contact :: TezosAddress
  , _phone :: Text 
  , _email :: Text
} deriving (Show)

data RealEstateOption = 
  RealEstateOption {
    property :: Property
    , premium :: Tezzies
    , expiration :: UTCTime
    , exercisePrice :: Tezzies
  } deriving (Show) 

data Contractor = Contractor {
  _contractorName :: Text 
  , _contractorAddress :: Text 
  , _licenseRenewedOn :: UTCTime
  , _licenseExpiresOn :: UTCTime
}deriving(Show)

{-| 
The tezzies expressed in terms of an __/amount/__ and
an __/exponent/__ that following general rules of currency 
computation and presentation. 
-}
-- TODO: implement the presentation of tezzies.
data Tezzies = Tezzies {
  _tAmount :: Integer, 
  _exponent :: Integer
} deriving (Show)

