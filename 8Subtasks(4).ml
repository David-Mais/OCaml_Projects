type 'a option = None | Some of 'a;;

let rec drop_last_opt l = match l with
|[] -> None
|[x] -> Some []
|h::t -> match drop_last_opt t with
  |None -> None
  |Some x -> Some (h::x);;

  let testing_drop_last_opt () =
    let l =
      [
        __LINE_OF__ ((drop_last_opt []) = None);
        __LINE_OF__ ((drop_last_opt [1]) = Some []);
        __LINE_OF__ ((drop_last_opt [1;2;3]) = Some [1;2])
      ] in
    let result = List.fold_left (&&) true (List.map snd l) in
    if result then (Printf.printf "The drop_last_opt test succeeds.\n"; [])
    else (Printf.printf "The drop_last_opt test fails.\n Check the corresponding line numbers in the list below.\n";
          (List.filter (fun (x,y) -> y=false) l) |> List.map fst);;