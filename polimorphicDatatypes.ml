type 'a tree = Leaf of 'a
  |Node of ('a tree * 'a tree);;
type 'a option = None | Some of 'a;;
let tree1 = Leaf 1;;
let tree2 = Node ((Node (Leaf 2, Leaf 3)),(Node (Leaf 4, Leaf 5)));;
let tree3 = Node ((Node (Leaf 2, Leaf 3)),(Node (Node (Leaf 55, Leaf 16), Leaf 5)));;
let tree4 = Node (Node (Leaf 1, Leaf 5), Leaf 3);;
let tree5 = Node (Node (Leaf ("Hello", true), Leaf ("good", true)), Leaf (("world", false)));;

let rec size = function
  |Leaf _ -> 1
  |Node (x, y) -> (size x) + (size y);;
let rec flatten = function
  |Leaf x -> [x]
  |Node (a, b) -> (flatten a) @ (flatten b);;
let flatten1 tr = let rec helper = function
  |(Leaf hd, tl) -> hd::tl
  |(Node (a,b), tl) -> let tl = helper (b, tl) in
  helper (a, tl) in helper (tr, []);;

type 'a queue = 'a list;;

let enqueue a q = q @ [a];;
let dequeue q = match q with 
  |[] -> (None, [])
  |hd::tl -> (Some hd, tl);;
let is_empty q = match q with
  |[] -> true
  |hd::tl -> false;;
let queue_of_list (lst : 'a list) : 'a queue = lst;;
let list_of_queue (lst : 'a list) : 'a queue = lst;;

type 'a queue2 = Queue of 'a list * 'a list;;
let is_empty2 q = match q with
  |([], []) -> true
  |_ -> false;;