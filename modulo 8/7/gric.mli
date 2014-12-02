type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
val foglie_costi : int tree -> (int * int) list
