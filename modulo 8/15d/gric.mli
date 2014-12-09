type 'a ntree = Empty | Tr of 'a * 'a ntree * 'a ntree
exception E
val abr_delmin : ('a * 'b) ntree -> ('a * 'b) * ('a * 'b) ntree
