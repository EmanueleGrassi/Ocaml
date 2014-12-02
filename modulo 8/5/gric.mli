type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
exception Errore
val segui_bool : bool list -> 'a tree -> 'a
