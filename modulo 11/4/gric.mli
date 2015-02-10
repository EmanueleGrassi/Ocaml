type 'a graph = ('a * 'a list) list
exception Nodo_inesistente
val vicini : 'a -> ('a * 'b) list -> 'b
val grafo_connesso : ('a * 'a list) list -> bool
