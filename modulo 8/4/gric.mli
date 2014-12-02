type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
val numero_foglie : 'a tree -> int
