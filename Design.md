# Realitz - Manage clean up of properties contaminted with oil.

## Requirements

The overarching goal of this application to allow for a real-estate investor, an environment contractor and the property owner to manage the 
various steps in the process of cleanup of properties contaminated with oil. From an environmental viewpoint, this presents a huge problem for counties
and municipalities around the world and managing real costs as well as transaction costs can only help in reducing the overall budget for cleanup. The block chain part of this solution is only part of the overall solution to help controlling the costs of cleanup. The other part is choice of the products proven on the field to controlling
the costs of the cleanup. This document obviously focuses on the software solution that we intend to build. 

## Platform

The application needs to run on 
	* Mobile phones
		* iPhone
		* Android

	* Desktop 
		* HTML5
		* Desktop clients if needed.


## Security constraints

Security concerns and auditability of the application require the application source code to be open source to help prevent 0-day defects. No external calls can be 
made without the private key being encrypted. (TODO: There must be some way to prevent any application deployments that violate this rule). 


## Choice of programming language 

This application will be implemented using Haxe as it has support for many of the features we need. 

## Forms design 

### Login screen 

### Events and alerts

### Property watch lists.
This screen captures all the events that the user has registered for. These events can include 
	* New properties that need investement within the investment rules.
	* Property reports on existing properties
	* Settlement 
	* Pullback - this is when the group decides that the investment is a loss needs to be pulled back or that the option has expired.

### Property details
The details of a given property including the last set of events on the property. Unlike a real-estate site this screen need not 
be as elaborate. This screen will capture the geo-location coordinates, and a small album with some images on the property. If video
feed has been setup as a security measure this should display a video feed. Though this is not a priority in the initial phase of the 
application.

### Request for investment phase
This round presents a list of properties for vote. Each property vote will contain at least the following parameters
	* The premium
	* The Strike price 
	* Current depressed market price. 
	* Realtor 
	* Jurisdiction of the property
	* Number of minimum investors needed to fund the project.
	* Contamination report on the property.
	* Start time of the option 
	* End time of the option.
	* Lawer representing the option.
	* Signatories for the option (subset of the investor list) 
	* Consensus count.
	* Go/NoGo.
	* Technical signoff
	* Current investment in Tezzies on the project. 

### Investment escrow account
	* Each property will be maintained by an escrow account that will be used to disburse funds for service 
	providers.  

#### Rules for an escrow account
	* Account transfers into service providers account can only be activated after the start date of the property.


### Contamination report
This report contains the current contamination report as well as the contamination history as some of the contaminations can 
span years. The summary of the report will contain the high level targets that need to be accomplished to achieve an NFA (no further action) certificate on the property.

### Technical signoff phase
	* A group of technical experts selected by the investment group, will signoff on the feasibility of cleanup within reasonable cost for the project. 
	At this point this expertise is mainly provided the solution provider referred to at the top of the document. Eventually, a vote on the feasibility of 
	project will also be a phase that will be part of the overall voting checks and balances. 

	* Date 
	* Signature of the expert (address).
#### Rules for technical signoff
	* The date of signoff should be less than the project start date.

### Recruitment phase
This is when the contract's conditions for insurance requirements and qualifications of the workers on the property are evaluated and recorded that 
the qualifications match the contract requirements.

### Contract requirements
Each option contract stipulates a set of requirements the contract holder is liable to execute. Sample requirements include, 
	* Proforma qualifications to perform the job onsite.
	* Insurance and liability requirements for each task and job.
	* Start date for the task
	* End date for the task
	* Authorization role for the task.

### Project planning and execution phase
All the task lists for the project. At this point, we dont see a need to vote on this, because by this point the expertise has been vetted. Also,
we need to remember that there is a physical project manager scrutinizing each action the application is only to record them so that investors have an 
accurate report of the status of the project. 
	* Task classification - if any
	* Task start date 
	* Task end date
	* Task rate list - approved rate list as per the local dep.
	* Task amount - rate list * number of days or hours depending on the rate list.
	* Task status

#### Task status
Much as in most task management systems,  
	* Started
	* Completed
	* Rejected 

### Contamination evaluation phase
Contamination report after the cleanup tasks have been completed.
	* Contamination CAS number, concentration measured, concentration threshold.

#### Evaluation of test results
	* Tests pass when all the contaminants have their respective concentrations below the allowable threshold.


### DEP report
The relevant state authority issued report on the property, which should read as an NFA.
	* Report status : NFA or Further action.
	* Report date.

#### Evaluation rules
	Report date should be less than the option date.


### Settlement phase
The property value is re-assed after the NFA phase is complete and published.  The members cast a vote 
to sell the property if a buyer approaches. Upon receiving a vote from all the investors, the subsequent phase will
have a off-chain component where physical documents will be generated for physical signatures to approve the sale. 
This also implies that the signatories on the contract have the authority to purchase and sell properties in the jurisdiction.
We are not <em> allowing </em> proxies in this application.

### Settlement phase II
The proceeds of the sale are sent as a check/wire to the signatories. The images of the check are maintained for recording the 
transaction.

#### Repeat.

### Mark-to-market accounting
Crypto currencies have unregulated fluctuations, therefore the process would settle the core account daily to manage risk. As a practice, the accounts will only contain monies to be paid
for the next 24 hours, to prevent any tezzies accruing interest on these accounts.

### Reference
Collect references in coq here : 
* [coq slides](https://www.lri.fr/~paulin/LASER/coq-slides4.pdf)
* [Typeclasses](http://people.csail.mit.edu/jgross/personal-website/papers/academic-papers-local/mathclasses-diamond.pdf)
* [Record types](http://www.konne.me/2015/02/04/data-abstraction-0.html)
