### User Interaction Stories

User of the application can be either a mobile or a desktop application. Here 
we are going to present the various interactions that cover the application usage.
We are also trying to classify operations as wallet or public domain depending on the 
sensitivity of the operation.

### 1TUX (first time user experience)
We are assuming that each user has a well known public hash. A user can register
as a result of being part of an existing deal or a new deal. Most of initial user base will be by invitation only. This addresses the KYC issues needed to legally form an entity managing the deal.

#### Existing deals
If the user is already part of some deals, then upon sign-on the user is presented
with a list of deals tied to the users' public key hash.

#### New deals
In this case all new properties that are waiting to be voted on.  Initially, 
this interaction is simple. The next versions of the application will enable
a risk profile so that deals that match the risk profile are presented to the user.

### General user experience
When a user logs in the user is presented with the following high level
list of actionable items.

#### Active deals (wallet operation)
This detail lists all the deals that the user has voted for and has committed to an investment.

#### Active proposals
This list all proposals that are up for vote.  
##### Proposal detail
Each proposal at least has the following criteria to make it to a vote:
  * Property coordinates
  * Strike/Exercise price of the option 
  * Premium for the option 
  * Potential cleanup costs 
    * Accompanied by a detailed estimate by an environment consultant or a geologist.
  * Timeline 
  * Potential value after cleanup.
  * Current history.
Note : Privacy concerns might be raised as property history has an impact on future sales. However, oil releases are recorded by the State Department of Environment Protection and therefore by nature available for public scrutiny. This screen on its own can provide useful 
information for any investor for any property managed by the application. 

#### Deals with NFA
This lists all the properties within the user's portfolio that have received an NFA (no further action). 

#### Deals with reassessment after cleanup
List of properties that are being reassessed after receving an NFA. Naturally, these properties will be only presented for a vote when the property has received an NFA and is current.

#### Active settlements
List of all properties that are under some form of contract. The settlement process is a place where we believe we have to leave the block chain for a small bit and re-enter after we get legal sign-off on the properties. This might be the place where the real world financial instruments cross paths with the blockchain and in many cases legally unavoidable. Because the investment group can decide to liquidate the entity formed to limit liability after a payout expecting an immediate payout. Or can decide to run the entity to collect rent on the property.


#### Active project work
Project work can only be done on a property by a licensed contractor with the appropriate insurance liability certificate attached for the work. Each task is presented to the user that lists the following :
  * Work order number
    * Start Date 
    * End Date
    * Details
    * Insurance Requirements in USD (or ideally tezzies)
    * Insurance certificate 
    * Number of units
    * Rate per unit
    * Total amount
    * Approved by (the members of the group approving this task).
  * Invoices
    * Work order Number
    * Amount to be paid
    * Payment authorized by (the members of the group approving this payment);
    * Invoice status
  * Project lab reports
    * Lab Id
    * Type of test 
    * Reference to the test protocol document 
    * Test date
    * Test data
      * Raw data that can be processed by researchers.
      * Printed (tamper proof format) document with the authorized signature for the DEP.
    * Test prepared by : Authorized user could be different from the authorized signature in the report, but still has to part of the list of approved laboratory personnel.
      
#### Active rentals
Any properties that were converted into a rental or lease agreement (not an immediate priority).

#### Past settlements without most likely the actual payout (more about that later)




### Wallet operations
Some contracts are not open for general public and require the user to enter their passphrase to access the details. Wallet operations encompass that part of the application. 




##### Glossary
Some jargon is un-avoidable in this field.

* NFA : No further action (in other words, take this to the bank).
* Current NFA : An nfa that is less than 3 years old.
