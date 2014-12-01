type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
val fulltree : int -> int tree
