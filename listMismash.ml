(*
* here are some lists to practice you can change numbers and quatity of components   
*)
let l1 = [1;2];;
let l2 = [5;6;7;8];;
let l3 = [9;10;11;12;13;14;15]

(*
* A recursive function that does the following
* [0;1;2], [10;11;12] and [20;21;22] the function must return the list [0;10;20;1;11;21;2;12;22]
* it takes fisrt elements of the lists by order then second elements that thirds
! if any of the list runs out of components then function continues with remaining lists   
*)
let rec interleave3 l1 l2 l3 = match l1, l2, l3 with
|[], [], l3 -> l3
|[], l2, [] -> l2
|l1, [], [] -> l1
|[], h2::t2, h3::t3 -> h2::h3::interleave3 [] t2 t3
|h1::t1, [], h3::t3 -> h1::h3::interleave3 t1 [] t3
|h1::t1, h2::t2, [] -> h1::h2::interleave3 t1 t2 []
|h1::t1, h2::t2, h3::t3 -> h1::h2::h3::interleave3 t1 t2 t3;;