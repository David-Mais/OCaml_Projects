let hd lst = match lst with
|[] -> failwith "empty list"
|hd::tl -> hd;;

let tl lst = match lst with
|[] -> failwith "empty lsit"
|hd::tl -> tl;;

let rec length lst = 
  let a = 0 in
  match lst with
  |[] -> a
  |hd::tl -> (a + 1) + length tl;;

let rec append lst1 lst2 = match lst1 with
  |[] -> lst2
  |hd::tl -> hd::append tl lst2;;
  
let rec rev lst = 
  let a = [] in
  match lst with
  |[] -> a
  |hd::tl ->append (rev tl) (hd::a);;

let rec nth lst n =
  if((length lst) < n) then failwith "list too short"
  else if(n < 0) then raise (Invalid_argument "n must be non-negative")
  else match n with
  |0 -> hd lst
  |_ -> nth (tl lst) (n-1);;