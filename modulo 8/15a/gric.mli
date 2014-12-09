type 'a ntree = Empty | Tr of 'a * 'a ntree * 'a ntree
val getinfo : ('a * 'b) ntree -> 'a
val abr_check : ('a * 'b) ntree -> bool
