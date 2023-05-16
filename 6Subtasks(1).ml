(*
  ? fold_left f1 [] [(a1, b1) ; ... ; (an, bn)] 
  ? for arbitrary ai, bi computes the list [(b1, a1); ... ; (bn, an) ]
*)
(* 
  * this funciton switches place to touple and adds the result on the back of the accumulator 
  * and when List.fold_left is applied every single component of the list is added in reverse
*)
let f1 lst (a,b) = lst @ [(b,a)];;

(*
  ? fold_left f2 [] [a_0 ; ... ; a_{n−3} ; a_{n−2}; a_{n−1}; a_n] 
  ? for arbitrary elements a_i computes the list [a_n; a_{n−2} ; ... ; a_0 ; ... ; a_{n−3} ; a_{n−1}]
*)
(*
  * now this function works in this way whenever you recieve element you add it in accumulator
  * and instantly reverse the acc whis way the function returns the list in a way that is wanted
  * try this algorithm on your own*)
let f2 acc x = match acc with
  |[] -> [x]
  |_ -> List.rev (acc @ [x]);;
(*
  ? fold_left f3 (fun _ -> 0) [(k1 , v1) ; ... ; (kn, vn) ] computes a function g such that 
  ? g(ki) = vi for all 1 ≤ i ≤ n. The k's are assumed to be pairwise distinct.
*)
(*
  ! try explaining f3 on your own
*)
let f3 acc (k, v) = fun x -> if x = k then v else acc x;;

let testing_fs () =
  let l =
    [
      __LINE_OF__ ((List.fold_left f1 [] [(1,2); (3,4); (5,6)]) =
                     [(2,1); (4,3); (6,5)]);
      __LINE_OF__ ((List.fold_left f2 [] ['a';'b';'c';'d';'e';'f';'g']) =
                     ['g';'e';'c';'a';'b';'d';'f']);
      __LINE_OF__ (let g = List.fold_left f3 (fun _ -> 0)
                             [('a',3); ('z', -9); ('d', 18)] in
                   (g 'a' = 3) && (g 'd' = 18) && (g 'z' = -9))
   ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The f1, f2, f3 test succeeds.\n"; [])
  else (Printf.printf "The f1, f2, f3 test fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst);;