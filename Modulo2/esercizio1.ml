(* Primo Esercizio*)
(* ultime_cifre è una funzione che dato un intero restituisce una coppia*)
(* contenente le ultime due cifre dell'intero dato*)
(*   ultime_cifre: int->int*int   *)

let ultime_cifre n = let m = abs n in (((m / 10) mod 10), (m mod 10));;
