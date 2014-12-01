type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
val drop : int -> 'a list -> 'a list
val take : int -> 'a list -> 'a list
val balpreorder : 'a list -> 'a tree
val balinorder : 'a list -> 'a tree
