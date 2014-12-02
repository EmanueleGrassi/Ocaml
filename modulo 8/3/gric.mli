type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
val foglie_in_lista : 'a list -> 'a tree -> bool
