type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
type 'a sostituzione = ('a * 'a tree) list
val gettree : 'a -> ('a * 'b tree) list -> 'b tree
val applica : ('a * 'a tree) list -> 'a tree -> 'a tree
