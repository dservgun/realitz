From mathcomp.ssreflect Require Import ssreflect.

Print True.

Theorem true_is_true : True.
exact: I.
Qed.

Theorem one_eq_two : False -> 1 = 2.
Proof.
  case.
Qed.

Theorem imp_transs : forall P Q R : Prop, (P -> Q) -> (Q -> R) -> P -> R.
Proof.
  move=> A B C.
  move=> H1 H2 a.
  exact: (H2 (H1 a)).
Qed.