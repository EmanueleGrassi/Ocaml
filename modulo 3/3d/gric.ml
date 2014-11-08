(* Esercizio 3d *)
(* fib: int->int *)
(* questa funzione calcola la sequenza di fibonacci del parametro n *)

let rec fib n =
	match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib(n - 1) + fib (n - 2)