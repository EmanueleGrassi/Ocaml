type 'a graph = ('a * 'a) list
exception NotFound
type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list
exception Non_Presente
val colore : 'a -> ('b * 'a list) list -> 'b
val vicini : 'a -> ('a * 'b) list -> 'b list
val colori_alterni :
  ('a * 'a) list -> ('b * 'a list) list -> 'a -> 'a -> 'a list
