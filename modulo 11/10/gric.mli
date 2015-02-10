exception NotFound
type chiave = Aperta | Chiusa
type cassaforte = chiave list
val vicini : 'a -> ('a * 'b) list -> 'b list
val giraChiave : chiave -> chiave
val giraPrima : chiave list -> chiave list
exception Operazione_non_eseguibile
val giraDopoChiusa : chiave list -> chiave list
val successori : chiave list -> chiave list list
val tutte_liste_con : int -> 'a -> 'a -> 'a list list
val nodi : int -> chiave list list
val archi : int -> (chiave list * chiave list list) list
val start : int -> chiave list
val aperta : chiave list -> bool
val apri : int -> chiave list list
