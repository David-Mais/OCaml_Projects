let f x = x + 5;;
let g y = let z = 7
  in if(y > 5) then f (-y)
  else z + f y;;
let rec fac1 = function
  |(1,acc) -> acc
  |(n,acc) -> fac1 (n-1,n*acc);;
let rec loop x = if(x < 2) then x
  else if(x mod 2 = 0) then loop (x/2)
  else loop (3*x + 1);;
let app list1 list2 = list1 @ list2;;
let rec rev_quad list = match list with
  |[] -> []
  |h::t -> app (rev_quad t) [h];;
let rev list = 
  let rec r a l = 
    match l with
    |[] -> a
    |h::t -> r (h::a) t
  in r [] list;;
let apply f a b = f (a,b);;
let plus (x,y) = x + y;;
let plus2 = apply plus 2;;
let plus3 = apply plus 3;;

let rec map f = function
  |[] -> []
  |h::t -> (f h)::map f t;;
let rec fold_left f a = function
  |[] -> a
  |h::t -> fold_left f (f a h) t;;
let rec fold_right f = function
  |[] -> fun b -> b
  |h::t -> fun b -> f h (fold_right f t b);;
let rec  find_opt f = function
  |[] -> None
  |h::t -> if f h then Some h
  else find_opt f t;;
let cons_r t h = h::t;;
let rev l = fold_left cons_r [] l;;
let compose f g x = f (g x);;
let twice f x = f (f x);;
let rec iter f g x = 
  if (g x) then x
  else iter f g (f x);;