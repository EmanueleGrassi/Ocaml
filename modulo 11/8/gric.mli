type 'a graph = ('a * 'a) list
exception NotFound
val vicini : 'a -> ('a * 'b) list -> 'b list
val esiste_cammino : ('a * 'a) list -> 'a -> 'a -> bool
val connessi_in_glist : ('a * 'a) list list -> 'a -> 'a -> bool
