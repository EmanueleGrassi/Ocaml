(* Esercizio 2*)
(* prodotto: nat -> nat -> nat*)
(* prodotto � la funzione che implementa la moltiplicazione mediante somma fra numeri*)
(* naturali rappresentati dal tipo 'nat' definito a lezione*)
(* La funzione somma: nat -> nat -> nat � quella che � stata definita a lezione *)

let rec prodotto n m =
	match m with
	| Zero -> Zero
	| Succ(Zero) -> n
	| Succ k -> somma n (prodotto n k)

