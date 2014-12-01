type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
val altezza : 'a tree -> int
val balanced : 'a tree -> bool
