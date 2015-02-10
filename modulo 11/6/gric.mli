type 'a graph = 'a list * ('a * 'a) list
exception NotFound
val remove : 'a -> 'a list -> 'a list
val vicini : 'a -> ('a * 'b) list -> 'b list
val cammino : 'a * ('b * 'b) list -> 'b list -> 'b -> 'b -> 'b list
val hamiltoniano : 'a list * ('a * 'a) list -> 'a list
