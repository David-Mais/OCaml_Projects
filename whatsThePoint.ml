(*
TODO 1) Define a suitable data type for your point.
TODO    type vector3 = (* TODO *)  
*)
(*
TODO 2) Define three points p1, p2 and p3 with different values.
*)
(*
TODO 3) Implement a function vector3_to_string : vector3 -> string to 
TODO    convert a vector into a human-readable representation.
*)
(*
TODO 4) Write a function vector3_add : vector3 -> vector3 -> vector3 to add two vectors.
TODO    let vector3_add = (* TODO *)   
*)
(*
TODO 5) Write a function vector3_max : vector3 -> vector3 -> vector3 that returns the larger vector (the vector with the greater magnitude).
TODO    let vector3_max = (* TODO *)   
*)
(*
TODO 6) Compute the result of adding p1 to the larger of p2 and p3 and print the result as a string.
        (* TODO *)   
*)

type vector3 = (int * int * int);;

let vector1 = (1, 2, 3);;
let vector11 = (1, 2, 3);;
let vector2 = (4, 5, 6);;
let vector3 = (7, 9, 9);;


let fst (x, _, _) = x;;
let snd (_, x, _) = x;;
let trd (_, _, x) = x;;
let vector3_to_string v =
  Printf.printf "(%d, %d, %d)" (fst v) (snd v) (trd v);;

let vector3_add v1 v2 = 
  ((fst v1 + fst v2), (snd v1 + snd v2), (trd v1 + trd v2));;

let magnitude v = 
  sqrt(float_of_int((fst v * fst v) + (snd v * snd v) + (trd v * trd v)));;

let vector3_max v1 v2 = 
  if(magnitude v1 > magnitude v2) then v1
  else if (magnitude v1 < magnitude v2) then v2
  else (0,0,0);;

let vector3_maxx v2 v3 = 
  if(magnitude v2 > magnitude v3)
  then vector3_to_string(vector3_add v2 vector1)
  else if(magnitude v3 > magnitude v2)
  then vector3_to_string(vector3_add v3 vector1)
  else ();;