let rec zip_with f l1 l2 = match l1, l2 with
|[], [] -> []
|_, [] -> []
|[], _ -> []
|h1::t1, h2::t2 -> (f h1 h2)::zip_with f t1 t2;;


let testing_zip_with () =
  let l =
    [
      __LINE_OF__ ((zip_with (fun x y -> [x;y]) [1;2;3] [5;6]) = [[1; 5]; [2; 6]]);
      __LINE_OF__ ((zip_with (fun x y -> [x;y]) [1;2;3] [5;6;7;8]) = [[1; 5]; [2; 6]; [3; 7]]);
      __LINE_OF__ ((zip_with (fun x y -> (x,y)) [1;2;3] ['a';'b']) = [(1, 'a'); (2, 'b')]);
      __LINE_OF__ ((zip_with (+) [1;2;3] [5;6]) =[6; 8]);
      __LINE_OF__ ((zip_with (^) ["aa";"bb";"cc"] ["1";"2"]) = ["aa1"; "bb2"]);

    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The zip_with test succeeds.\n"; [])
  else (Printf.printf "The zip_with test fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst);;