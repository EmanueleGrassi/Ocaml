type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
exception E
val remove_elem : 'a -> 'a list -> 'a list
val path_coprente : 'a tree -> 'a list -> 'a list
