From mathcomp.ssreflect Require Import ssreflect.
Require Import List ZArith Bool.

Open Scope Z_scope.


Record Date : Type := 
  {year : Z; month : Z ; day : Z}.