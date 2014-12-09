type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
val stessa_struttura : 'a tree -> 'b tree -> bool
val crea_lista : 'a tree -> 'b tree -> ('a * 'b) list
val esiste_mapping : 'a tree -> 'b tree -> bool
