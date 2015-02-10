exception NotFound
type 'a graph = ('a * 'a) list
val vicini : 'a -> ('a * 'b) list -> 'b list
val path_n_p : ('a * 'a) list -> ('a -> bool) -> int -> 'a -> 'a list
