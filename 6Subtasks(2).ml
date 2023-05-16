(*
  ! Rewrite the following functions in a tail-recursive form:*)
(*
  ? let rec map f = function
  ? | [] -> []
  ? | x :: xs -> f x :: map f xs
*)
(*
  ? let rec replicate n x =
  ?   if n < 1 then [] else x :: replicate (n-1) x
*)

(*
  * we define map function in tail recursive way. we recieve function and a list but in order
  * to write function tail recursively we need accumulator so we define helper function inside
  * map_tr that has accumulator and adds the affected elements of initial list into accumulator
  * as soon as initial list is empty the helper returns accumulator and so does the map_tr
*)
let map_tr f l = 
  let rec helper f l acc = match l with
    |[] -> acc
    |hd::tl -> helper f tl (acc @ [f hd])
  in helper f l [];;

(*
  * we define replicate function in a tail recursive way. we recieve a number of times element
  * should be replicated and an element of generic type meaning it can replicate integers strings...
  * then we patterns match n so that whenever it reaches 0 we should return accumulator but if it matches
  * with anyting else we call helper function with (n-1) the x (element we should replicate) and accumulator
  * with element x applied
*)
let replicate_tr n x = 
  if (n < 0) then [] else
  let rec helper n x acc = match n with
    |0 -> acc
    |_ -> helper (n-1) x (x::acc)
  in helper n x [];;


  let test_tr_llist () =
    let l =
      [
        __LINE_OF__ (map_tr succ [1;2;3] = [2;3;4]);
        __LINE_OF__ (map_tr (fun x -> x^x) ["a";"b";"c"] = ["aa";"bb";"cc"]);
        __LINE_OF__ (replicate_tr 5 "a" = ["a";"a";"a";"a";"a"]);
        __LINE_OF__ (replicate_tr (-3) "a" = [])
      ] in
    let result = List.fold_left (&&) true (List.map snd l) in
    if result then (Printf.printf "The tests for map and replicate succeed.\n"; [])
    else (Printf.printf "The test for tests for map and replicate fail.\n Check the corresponding line numbers in the list below.\n";
          (List.filter (fun (x,y) -> y=false) l) |> List.map fst);;