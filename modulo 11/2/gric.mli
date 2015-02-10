type 'a graph = ('a * 'a) list
exception NotFound
val vicini : 'a -> ('a * 'b) list -> 'b list
val esiste_ciclo : ('a * 'a) list -> 'a -> bool
