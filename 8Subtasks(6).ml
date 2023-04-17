let unzip list1 = 
  List.fold_right (fun (h1,h2) (t1,t2) -> (h1::t1, h2::t2)) list1 ([],[]);;

  let testing_unzip () =
    let l =
      [
        __LINE_OF__ ((unzip [('a',1); ('b',2)]) = (['a';'b'], [1;2]));
        __LINE_OF__ ((unzip []) = ([], []));
        __LINE_OF__ ((unzip [('a',1)]) = (['a'], [1]));
 
      ] in
    let result = List.fold_left (&&) true (List.map snd l) in
    if result then (Printf.printf "The unzip test succeeds.\n"; [])
    else (Printf.printf "The unzip test fails.\n Check the corresponding line numbers in the list below.\n";
          (List.filter (fun (x,y) -> y=false) l) |> List.map fst);;