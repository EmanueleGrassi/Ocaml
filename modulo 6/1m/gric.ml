(* Esercizio 1m *)
(* permutations: 'a list -> 'a list list *)
(* è una funzione che genera una lista di liste nella quale ogni lista è una*)
(* possibile permutazione della lista passata come parametro*)
(* Questa funzione utilizza la funzione interleave definita nell'esercizio*)
(* 1l del modulo 6*)

let rec interleave e =
  function
  | [] -> [[e]]
  | (x::rest as lst) ->
      (e::lst) :: (List.map (fun y -> x :: y) (interleave e rest))

let rec permutations lst =
  match lst with
  | x::rest -> List.concat (List.map (interleave x) (permutations rest))
  | _ -> [lst]
