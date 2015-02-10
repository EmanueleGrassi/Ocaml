type form =
    Prop of string
  | Not of form
  | And of form * form
  | Or of form * form
exception NotFound
type 'a graph = ('a * 'a) list
val vicini : 'a -> ('a * 'b) list -> 'b list
val complementare : form -> form
val non_contradictory_path : (form * form) list -> form -> form -> form list
