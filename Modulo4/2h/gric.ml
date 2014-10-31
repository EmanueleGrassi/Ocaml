(* Esercizio 2h *)

exception Empty_List
  
let maxlist =
  function
  | [] -> raise Empty_List
  | x :: rest ->
      let rec aux x rest =
        (match rest with | [] -> x | x1 :: rest1 -> aux (max x x1) rest1)
      in aux x rest
  
let min_dei_max =
  function
  | [] -> 0
  | x :: rest0 ->
      let maxtmp = maxlist x in
      let rec aux max =
        (function | [] -> max | x :: rest -> aux (min max (maxlist x)) rest)
      in aux maxtmp rest0
  
let rec min_dei_max_coda =
  function
  | [] -> 0
  | [ x ] -> maxlist x
  | x :: rest -> min (maxlist x) (min_dei_max_coda rest)
  
