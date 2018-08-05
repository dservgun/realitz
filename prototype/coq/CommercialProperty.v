(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) Dinkar Ganti, dinkar.ganti@gmail.com                        *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

Require Import List ZArith Bool String.
Require Export ZArith_base.
Require Export Coq.Reals.Rdefinitions.
Require Import Coq.Lists.List.
From mathcomp Require Import all_ssreflect.
Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

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

(** The address and location for the property. TODO: assign a risk score for the
property. *)
Record property : Type := Property {address : string}.

(** Various tasks needed to completed on the property. These could involve *)
Record task : Type := Task {taskId : Z; 
  taskDescription : string;
  rate : R;
  numberOfUnits : R
  }.

Record budget : Type := Bugdet {
  amount : R;
}

(** Prove that the sum of all tasks is less than or equal to the amount in the budget*)


Record edge : Type := Edge {
  from : Task; 
  to : Task
}.

Record project : Type := Project {
  tasks : list Edge
}.

(** Lemma : No task left behind. Given a project no task should be executed that has a
predecessor pending or not approved *)

(** Lemma : No cycles in the task*)

(** Investor exposure*)
(** A given investor will usually have a risk appetite that will be computed based on the 
risk profile. Additionally, each transaction will need to go through 
a pre-approval process to help cover for any losses in existing investments.*)

Record InvestorExposure : Type := InvestorExposure {
  exposure : R;
  properties : list Property
}.

