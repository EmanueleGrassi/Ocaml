type 'a ntree = Empty | Tr of 'a * 'a ntree * 'a ntree
exception E
val abr_search : ('a * 'b) ntree -> 'a -> 'b
