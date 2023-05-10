let squaresnum lst = List.fold_left (+) 0 (List.map (fun x -> x * x) lst);;

let float_list lst = List.map (fun x -> float_of_int x) lst;;

let to_string lst = (String.sub ("[" ^ List.fold_left (^) "" (List.map (fun x -> (string_of_int x) ^ ";") lst)) 0
(String.length ("[" ^ List.fold_left (^) "" (List.map (fun x -> (string_of_int x) ^ ";") lst)) - 1)) ^ "]";;

let part_even lst = (List.filter (fun x -> (x mod 2) = 0) lst) @ 
                    (List.filter (fun x -> (x mod 2) = 1) lst);;