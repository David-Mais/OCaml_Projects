let f1 lst (a,b) = lst @ [(b,a)];;
let f2 acc x = match acc with
  |[] -> [x]
  |_ -> List.rev (acc @ [x]);;
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