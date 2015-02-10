val find_applicata : int list -> int
val takewhile : ('a -> bool) -> 'a list -> 'a list
val dropwhile : ('a -> bool) -> 'a list -> 'a list
val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
val pairwith : 'a -> 'b list -> ('a * 'b) list
val verifica_matrice : 'a -> 'a list list -> bool
val setdiff : 'a list -> 'a list -> 'a list
val subset : 'a list -> 'a list -> bool
val duplica : int list -> int list
val mapcons : ('a * 'b list) list -> 'b -> ('a * 'b list) list
val cons : 'a -> 'a list -> 'a list
val tutte_liste_con : int -> 'a -> 'a -> 'a list list
val interleave : 'a -> 'a list -> 'a list list
val permut : 'a list -> 'a list list
type 'a mat = int * ((int * int) * 'a) list
val labirinto : int * ((int * int) * string) list
val trova_colonna : 'a * (('b * 'c) * 'd) list -> 'b -> 'd -> 'c
val upto : int -> int -> int list
val in_riga : 'a * (('b * 'c) * 'd) list -> 'b -> 'd -> bool
val in_tutte : int * ((int * 'a) * 'b) list -> 'b -> bool
val find : 'a -> 'a list -> 'a list * 'a list
val spezza : 'a -> 'a list -> 'a list * 'a list
val prendi : ('a -> bool) -> 'a list -> 'a * 'a list
