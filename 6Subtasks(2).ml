let map_tr f l = 
  let rec helper f l acc = match l with
    |[] -> acc
    |hd::tl -> helper f tl (acc @ [f hd])
  in helper f l [];;

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