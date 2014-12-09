type 'a ntree = Empty | Tr of 'a * 'a ntree * 'a ntree
val abr_insert : ('a * 'b) ntree -> 'a * 'b -> ('a * 'b) ntree
val inorder : ('a * 'b) ntree -> 'a list
val tree_sort : 'a list -> 'a list
