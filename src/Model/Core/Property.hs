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
import Model.Core.Types
import Model.Core.Technician
import Network.URL

{-|
  = Remediation of properties contaminated with oil.
  The process of remediation of contaminated properties has quite a few
  steps while providing an investment opportunity to entities with an 
  appetite for risk. This application supports a model where the risk is limited 
  to cleanup costs, which in themselves can be quite significant, and not to the cost
  of acquiring a given property. 
  == Request for investment phase.

  During this phase, the group short lists a set of properties that meet a particular risk profile. 

  == Research phase 

  During this phase all the relevant environment reports need to be collected for investors to vote on the project. This phase also includes a phase to send invites to an investor to solicit interest in the project. Initially, 
  this can be a simple mailing list and a voting phase with the quorum of investors needed for the property.

  == Offer phase 

  During this phase, an offer is made to the seller that usually includes an option to purchase the property at a price before an agreed upon date. The option has a premium as well as a list of tasks the option holder agrees to perform, which in this case involves cleaning up the property during before the option expires. Obviously, the profitability of the deal is influenced by a number of factors and this document will outline some of the parameters that need to be managed during the active phase of a project.

  == Share certificate issue phase 
  Paper shares, that are legally binding, will be issued proportional to the tezzies invested 
  in the property. This is when the investment needed for the cleanup of the property is transferred
  to the cleanup account associated with the property. At this point the amounts are NON-REFUNDABLE.
  Which is generally true on a block chain, in this case since there is an associated paper transaction 
  that is legally binding, there could be a clawback in the event of company contract violations.

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



{-| The state the property is listed in, should be one of the 51 states in United States. The risk
associated with a contaminated is sufficiently to limit the boundary to within the United States.
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


{-| The option on the property granting __\limited\__ cleanup rights on the property
along with the time horizon for the option. This option is binding in the local jurisdiction
or within the boundaries of a Country. Therefore, such options can most likely be executed
by members forming an entity that is liable for cleanup. The liability, at least in the 
USA is usually limited therefore the partners in the entity can decide to suspend cleanup 
at anytime during the option period. Moreover, since the option stipulates acquiring a
liability insurance for the property for the duration of the option any ensuing risks are offset.
-}
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
an __/exponent/__ ; general rules of currency 
computation and presentation apply. 
-}
-- TODO: implement the presentation of tezzies.
data Tezzies = Tezzies {
  _tAmount :: Integer, 
  _exponent :: Integer
} deriving (Show)


{-| 
  This is a representation of the physical certificate issued by the company 
  accountant that is legally binding on the entity. The increased transparency helps
  ease the investor concerns as the shares of all investors is accessible.
-}
data InvestorShareCertificate = InvestorShareCertificate {
  _investor :: ContactInformation
  , _investorAddress :: TezosAddress
  , _investmentAmount :: Tezzies
  , _propertyShare :: Rational 
  , _issueDate :: UTCTime
  , _certificateLocation :: URL
} deriving (Show)

newtype USD = USD {_unUSD :: Rational}
{-|
  Settlement is when the sale has concluded on behalf of the investors and is usually 
  in cash. This is where the transaction steps off the block-chain and settles in local 
  currency. Ideally this step could have been executed on the block chain, however, 
  the value of these transactions could be large and errors can be devastating.
-}
data CertificateSettlement = CertificateSettlement {
  _investorShareCertificate :: InvestorShareCertificate
  , _distributionAmount :: USD
  , _distributionDate :: UTCTime
}


