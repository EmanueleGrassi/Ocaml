type 'a graph = ('a * 'a) list
exception NotFound
val remove : 'a -> 'a list -> 'a list
val vicini : 'a -> ('a * 'b) list -> 'b list
val cammino_con_nodi : ('a * 'a) list -> 'a -> 'a list -> 'a list
