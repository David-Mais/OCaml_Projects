(*
TODO Implement a function interleave3 : 'a list -> 'a list -> 'a list -> 'a list 
TODO that interleaves three lists. So for input lists [0;1;2], [10;11;12] and [20;21;22] 
TODO the function must return the list [0;10;20;1;11;21;2;12;22]. If a list is out of 
TODO elements (e.g. for inputs of different lengths), the function continues interleaving 
TODO the remaining lists, such that for inputs ['a';'b'], ['A';'B';'C';'D'] and ['!'] the 
TODO output ['a';'A';'!';'b';'B';'C';'D'] is produced.   
*)

let l1 = [1;2;3];;
let l2 = [1;2;3];;
let l3 = [1;2;3];;

(*
* this is another inplementation fo the program that i have already written
* unlike the previous program here everything is written recursively and i didn't have to type
* every case on my own
! it is aslo important that this program uses somthing like nested functions
* funcion interleave3 is defined with function interleave2   
*)
let rec interleave3 l1 l2 l3 =
  let rec interleave2 l1 l2 =
    match l1 with
    [] -> l2
    |head::tail -> head::interleave2 l2 tail
  in
  match l1 with
  |[] -> interleave2 l2 l3
  |head::tail -> head::interleave3 l2 l3 tail;;

  ()