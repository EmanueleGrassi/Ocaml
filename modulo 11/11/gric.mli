exception NotFound
type 'a graph = ('a * 'a) list
val vicini : 'a -> ('a * 'b) list -> 'b list
val isPrime : int -> bool
val cammino_di_primi : (int * int) list -> int -> int -> int list
