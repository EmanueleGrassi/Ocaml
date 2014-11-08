(* Esercizio 2h *)
(* split2 è una funzione che presa una lista restituisce una coppia di liste*)
(* in cui ogni elemento della coppia è una porzione della lista parametro*)
(* split2: 'a list -> 'a list * 'a list *)

let rec drop n lst =
  try if n = 0 then lst else drop (n - 1) (List.tl lst)
  with | Failure "tl" -> []
  
let rec take n =
  function
  | [] -> []
  | x :: xs -> if n <= 0 then [] else x :: (take (n - 1) xs)
  
let split2 lst =
  let half = (List.length lst) / 2
  in ((take half lst), (drop half lst))
  
