type 'a ntree = Empty | Tr of 'a * 'a ntree * 'a ntree
exception E
val min : 'a ntree -> 'a
val abr_delmin : ('a * 'b) ntree -> ('a * 'b) * ('a * 'b) ntree
val abr_delete : ('a * 'b) ntree -> 'a -> ('a * 'b) ntree
