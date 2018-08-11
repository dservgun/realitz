Require Import Coq.Lists.List.
Require Import Io.All.
Require Import Io.System.All.
Require Import ListString.All.
Require Import Coq.ZArith.ZArith.

Import ListNotations.
Import C.Notations.

Definition hello_world(argv : list LString.t) : C.t System.effect unit :=
    System.log(LString.s "Hello world!").

Definition your_name (argv : list LString.t) : C.t System.effect unit :=
    do! System.log (LString.s "What is your name ?") in
    let! name := System.read_line in
    match name with 
      | None => ret tt
      | Some name => System.log(LString.s "Hello " ++ name ++ LString.s "!")
    end.

Definition uname (argv : list LString.t) : C.t System.effect unit :=
    let! os := System.eval [LString.s "uname"; LString.s "-o"] in 
    let! machine := System.eval [LString.s "uname"; LString.s "-m"] in
    match (os , machine) with
    | (Some (0%Z, os, _), Some (0%Z, machine, _)) =>
      do! System.log (LString.s "OS: " ++ LString.trim os) in
      System.log (LString.s "Machine: " ++ LString.trim machine)
    | _ => ret tt
    end.

Definition run_your_name_ok (argv : list LString.t) (name : LString.t)
  : Run.t (your_name argv) tt.
eapply Run.Let.
apply (Run.log_ok (LString.s "What is your name ?")).
eapply Run.Let.
apply (Run.read_line_ok name).
apply (Run.log_ok (_ ++ name ++ _)).
Defined.
