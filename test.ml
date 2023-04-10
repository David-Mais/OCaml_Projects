let rec fact n = match n with
0 -> 1
|_ -> n * fact (n-1);;

let rec len l = match l with
[] -> 0
|h::t -> 1 + len t;;

let rec app l1 l2 = match l1 with
[] -> l2
|h::t -> h::app t l2;;
 
let factorial n = let rec iter x y = 
  if(x > n) then y 
  else iter (x + 1) (y * x) in
  iter 2 1;;

type color = Diamonds | Hearts | Spades | Clubs;;
type value = Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace;;

let to_string_color = function
Diamonds -> "Diamonds"
|Hearts -> "Hearts"
|Spades -> "Spades"
|Clubs -> "Clubs";;