(* Terzo Esercizio *)
(* data: int*string->bool *)
(* i parametri di data rappresentano un giorno e un mese dell'anno.*)
(* data verifica che essi rappresentino una data ammissibile in un anno non-bisestile *)
(* una data non è ammissibile(quindi la funzione viene valutata false) se: *)
(* -il giorno è inammissibile (es 47) *)
(* -il mese è inesistente (es marzio) *)

let data (d, m) =
  match m with
  | "gennaio" | "marzo" | "maggio" | "luglio" | "agosto" | "ottobre" | "dicembre" -> d <= 31
  | "aprile" | "giugno" | "settembre" | "novembre" -> d <= 30
  | "febbraio" -> d <= 28
  | _ -> false;;
  
