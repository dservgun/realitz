From mathcomp Require Import all_ssreflect.
Set Implicit Arguments.
Unset Strict Implicit.

About nat.
Check nat.
Print nat.

Lemma leqn0 (n : nat) : (n <= O) = (n == O).
Proof.
  by case: n => [| k].
Qed.  

Fixpoint muln (m n : nat) : nat := 
  if m is p.+1 then n + muln p n else O.

Lemma mulnO n : n * O = O.
Proof.
  by [].
Qed.

Lemma muln_eqO m n : (m * n == O) = (m == O) || (n == O).
Proof.
  case: m => [|m] //.
  case: n => [|n] //.
    rewrite mulnO.
    by [].
Qed.

Lemma leqE m n : (m <= n) = (m - n == O).
Proof.
  by [].
Qed.


Lemma leq_mul21 m n1 n2 : 
  (m * n1 <= m * n2) = (m == 0) || (n1 <= n2).
  Proof.
    rewrite !leqE -mulnBr muln_eqO.
    by [].
  Qed.

(** Quantifiers *)
Lemma seq_eq_ext (s1 s2 : seq nat) :
  size s1 = size s2 -> 
    (forall i : nat, nth O s1 i = nth O s2 i) -> 
      s1 = s2.

Definition commutative (S T : Type) (op : S -> S -> T) :=
    forall x y , op x y = op y x.

(* Organizing proofs with sections *)
Lemma example m p : 
  prime p -> p %| m `! + 1 -> m < p.