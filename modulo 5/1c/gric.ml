(* Esercizio 1c *)
(* split: ('a * 'b) list -> 'a list * 'b list *)
let split_coda lst =
  let rec aux tmp = function
    | [] -> ((List.rev (fst tmp)), (List.rev (snd tmp)))
    | (x, y) :: rest -> aux ((x :: (fst tmp)), (y :: (snd tmp))) rest
  in aux ([], []) lst
  
let rec split = function
  | [] -> ([], [])
  | (x, y) :: rest -> ((x :: (fst (split rest))), (y :: (snd (split rest))))
  
