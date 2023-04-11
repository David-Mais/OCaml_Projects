let li = [5;13;2;7];;


let rec split ml ll rl = match ml with
|[] -> (ll, rl)
|h::t -> split (List.rev t) rl (h::ll);;

let rec merge l1 l2 = match (l1, l2) with
|(_, []) -> l1
|([], _) -> l2
|(h1::t1, h2::t2) -> if(h1 < h2) 
  then h1::merge t1 l2
  else h2::merge l1 t2;;

  let rec mergesort l =
    match l, split l [] [] with
    | [x],_ -> [x]
    | l, (left, right) -> merge (mergesort left) (mergesort right);;