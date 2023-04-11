(*
? The natural numbers can be defined recursively as follows:

! 0 is a natural number.
! if n is a natural number, then so is the successor of 
*)

(** We can easily represent this in OCaml using corresponding constructors: *)
type nat = Zero | Succ of nat;;

(*TODO int_to_nat : int -> nat*)
(*TODO converts an integer to natural*)
let rec int_to_nat n = match n with
|0 -> Zero
|_ -> Succ (int_to_nat (n-1));;

(*TODO  nat_to_int : nat -> int*)
(*TODO converts a natural to integer.*)
let rec nat_to_int nat = match nat with
|Zero -> 0
|Succ t -> 1 + nat_to_int t;;

(*TODO add : nat -> nat -> nat No tests*)
(*TODO adds two natural numbers.*)
let rec add nat1 nat2 = match nat1 with
|Zero -> nat2
|Succ t -> add t (Succ nat2);;

(*TODO mul : nat -> nat -> nat*)
(*TODO multiplies two natural numbers*)
let rec mul nat1 nat2 = match nat1 with
|Zero -> Zero
|Succ t -> add nat2 (mul t nat2);;

(*TODO pow : nat -> nat -> nat*)
(*TODO a call pow a b computes a^b*)
let rec pow nat1 nat2 = match nat2 with
|Zero -> Succ Zero
|Succ t -> mul nat1 (pow nat1 t);;

(*TODO leq : nat -> nat -> bool*)
(*TODO a call leq a b computes a≤b*)
let rec leq nat1 nat2 = match nat1, nat2 with
|Succ x1, Succ x2 -> leq x1 x2
|_ -> nat1 = Zero;;