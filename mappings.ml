type 'a option = None | Some of 'a;;
let rec is_empty (lst : ('k * 'v) list) =
  match lst with
  |[] -> true
  |hd::tl -> false;;

let rec get k lst = match lst with
  |[] -> None
  |(x,y)::tl -> if(k = x) then Some y
  else get k tl;;

let rec put k v lst = (k,v)::remove k lst;;

let rec contains_key k lst = if (get k lst) = None then false else true;;

let rec remove k lst = match lst with
  |[] -> []
  |(x,y)::tl -> if (k = x) then remove k tl
  else (x,y)::remove k tl;;

let rec keys lst = match lst with
  |[] -> []
  |(x,_)::tl -> x::keys tl;;

let rec values lst = match lst with
  |[] -> []
  |(_,x)::tl -> x::values tl;;