type obj = Miss | Cann | Barca
type situazione = obj list * obj list
type azione = From_left of obj list | From_right of obj list
val count : obj list -> int * int
val safe : obj list * obj list -> bool
val remove_elem : 'a -> 'a list -> 'a list
val remove_lst : 'a list -> 'a list -> 'a list
val sposta : obj list * obj list -> azione -> obj list * obj list
exception Operazione_non_consentita
exception Operazione_non_sicura
val applica : azione -> obj list * obj list -> obj list * obj list
val actions : azione list
val from_sit : obj list * obj list -> (obj list * obj list) list
