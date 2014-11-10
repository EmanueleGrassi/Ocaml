(* Esercizio 8b*)
(* quicksort: 'a list -> 'a list *)
(* quicksort è una funzione che ordina la lista passata come parametro*)
(* dividendola, in ogni chiamata ricorsiva, in due gruppi, *)
(* in ogni dei quali ci sono rispettivamente tutti gli elementi*)
(* minori e maggiori di x, infine le varie sottoliste create vengono unite*)
(* per ottenere la lista riordinata *)
let rec quicksort =
  function
  | [] -> []
  | x::rest -> let small = List.filter ((>) x) rest
               and large = List.filter ((<) x) rest
  						 in (quicksort small) @ (x::(quicksort large))
  
