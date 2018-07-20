Require Import List ZArith Bool String.

(**
Remediation of properties contaminated with oil and associated volatile organic compounds (VOCS) present a unique challenge as well as an investment opportunity to investors with an appetite for risk. The goal of this application is to streamline as well as document all of the steps from the inception of the project to settlement. The overall goal of the application is to manage the costs of documenation as well as to provide local regulatory authority with the progress on a property transparently. 
*)

(** * Application lifecycle *)
(** The application needs to address the needs of various participants or actors and this section 
outlines the various phases as well as the interaction needs where obvious. *)
 
(** Milestone *)
(** A milestone is, as the name suggests a milestone in the project along with the the progress state for the project. 
In this application, we would like to require that accomplishing a milestone be set to vote as well as expert opinion, for example 
the relevant DEP case manager*)

(** Risk profile *)
(** A hueristics summarizing the risk on the property using generally acceptable accounting practices. Additionally, 
the risk profile is a milestone for a property, therefore is a contract requiring a quorum of evaluators reaching consensus. *)
(** Group protocol *) 
(** Membership to a grop is either by an invite or user driven *)
(** Pre-investment tasks *)
(** During this phase a group shortlists a set of properties that meet a particular risk profile *)

(** Research phase *)
(** During this phase the relevant reports on the property are subject to evaluation to provide the group with a preliminary analysis
 of the amount of contamination and the associated risk. This report may be followed up with a detailed report prepared by an 
environmental consultant to highlight any areas missed in the preliminary report. This is a phase where most investors will usually decide 
as to continuing on the property or otherwise*)

(** Offer phase *)
(** The group presents an offer to purchase an option on the property with limited access rights and cleanup rights on the property. Acquiring an option transfers the right to 
buy a property at an agreed price before the option expires. This contract is analogous to the 
popular options however the group takes on the role of both the market maker as well as the 
option writer. Which essentially implies that the risk of acquiring the option is transferred for a specified period. *)

(** Pre-cleanup phase *)
(** During this phase all of the contractors along with fall-back contractors are identified 
and assigned to the project. Each contractor has a liability requirement that needs to be met before the contractor can work on any given property. This phase formalizes the process and each contractor arrangement for the property is documented in this phase. 
*)

(** Project planning *)
(** Each project is divided into task that has a rate and number of units associated with it. Moreover, each task has a start date, end date with [WorkAuthorization] and [WorkInspection] to release [Payment]s worker.
*)

(** Investor exposure*)
(** A given investor will usually have a risk appetite that will be computed based on the 
risk profile. Additionally, each transaction will need to go through a pre-approval process to help cover for any losses in existing investemnts*)



