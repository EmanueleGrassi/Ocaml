type 'a graph = ('a * 'a) list
type obj = Miss | Cann | Barca
type situazione = obj list * obj list
val goal : 'a list * 'b -> bool
