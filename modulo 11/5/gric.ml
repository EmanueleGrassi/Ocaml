(* Esercizio 5 *)

type 'a graph = ('a * 'a) list
type obj = | Miss | Cann | Barca
type situazione = ((obj list) * (obj list))

let rec goal sit = (fst sit) = []