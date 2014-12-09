type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
exception E
val path : ('a -> bool) -> 'a tree -> 'a list
