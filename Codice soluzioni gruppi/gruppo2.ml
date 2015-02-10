(*============ Es1:ultime_cifre =============*)

let ultime_cifre x =
  let y = abs x in
  ((y/10) mod 10, y mod 10)

(*============ Es2:numeri-belli =============*)

let rec bello n =
  if n > -10 && n < 10 (* n e' di una sola cifra *)
  then match abs n with
    0 | 3 | 7 -> true
  | _ -> false
  else (* n e' di piu' cifre *)
    let (penultima,ultima) = ultime_cifre n
    in bello ultima && not (bello penultima)

(** qui sono utilizzati i "pattern multipli":
   anziche' scrivere
           0  -> true
         | 3  -> true
         | 7 -> true
   i diversi casi che hanno la stessa "parte destra" possono
   essere compattati in un unico caso con piu' pattern a sinistra
*)
(*============ Es3:data =============*)

let data (d,m) = 
  d>0 &&
  match m with
    "novembre" | "aprile" |"giugno" | "settembre" -> d<=30
  | "febbraio" ->  d<=28
  | "gennaio"|"marzo"|"maggio"|"luglio"|"agosto"|"ottobre" -> d<=31
  | _ -> false

