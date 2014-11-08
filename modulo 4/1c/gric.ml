(* Esercizio 1c *)
(* maxlist è una funzione che calcola il massimo elemento di una lista *)
(* maxlist: 'a list -> 'a *)
(* usa la funzione aux: 'a -> 'a list -> 'a *)
(* che utilizza 'a come massimo temporaneo *)
exception Empty_List
  
let maxlist =
  function
  | [] -> raise Empty_List
  | x :: rest ->
      let rec aux x rest =
        match rest with 
				| [] -> x 
				| x1 :: rest1 -> aux (max x x1) rest1
      in aux x rest
  
