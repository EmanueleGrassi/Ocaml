type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list
type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
exception Non_Presente
val colore : 'a -> ('b * 'a list) list -> 'b
exception Non_Esiste
val getinfo : 'a tree -> 'a
val path_to : 'a -> ('b * 'a list) list -> 'a tree -> 'a list
