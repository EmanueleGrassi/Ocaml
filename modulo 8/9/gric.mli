type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
val max_common_subtree : string tree -> string tree -> string tree
