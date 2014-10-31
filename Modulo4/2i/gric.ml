(* Esercizio 2h *)

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
  
