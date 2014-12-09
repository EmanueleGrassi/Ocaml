type 'a ntree = Empty | Tr of 'a * 'a ntree * 'a ntree
val abr_update : ('a * 'b) ntree -> 'a * 'b -> ('a * 'b) ntree
