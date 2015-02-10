type 'a graph = ('a * 'a) list
exception NotFound
val vicini : 'a -> ('a * 'a) list -> 'a list
val test_connessi : ('a * 'a) list -> 'a -> 'a -> bool
