val in_labirinto : int -> int * int -> bool
val find_content : ('a * 'b list) list -> 'b -> 'a list
val filter_vicini : int -> (int * int) list -> (int * int) list
val raccolti : ('a * 'b list) list -> 'b list -> 'a list
val raccolti_bis : ('a * 'b list) list -> 'b list -> 'a list
val combine : 'a list -> 'b list -> ('a * 'b) list
val split : ('a * 'b) list -> 'a list * 'b list
val cancella : 'a -> ('a * 'b) list -> ('a * 'b) list
val setadd : 'a -> 'a list -> 'a list
val union : 'a list -> 'a list -> 'a list
val intersect : 'a list -> 'a list -> 'a list
val setminus : 'a list -> 'a list -> 'a list
val subset : 'a list -> 'a list -> bool
val explode : string -> char list
val implode : char list -> string
val upto : int -> int list
val pairwith : 'a -> 'b list -> ('a * 'b) list
val pair : 'a -> 'b -> 'a * 'b
val intpairs : int -> (int * int) list
val trips : 'a list -> ('a * 'a * 'a) list
val take : int -> 'a list -> 'a list
val choose : int -> 'a list -> 'a list list
val strike_and_ball : 'a list -> 'a list -> int * int
val insert : 'a -> 'a list -> 'a list
val insort : 'a list -> 'a list
val partition : 'a list -> 'a -> 'a list * 'a list
val quick : 'a list -> 'a list
