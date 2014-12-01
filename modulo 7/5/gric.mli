exception Lunghezze_non_uguali
type 'a pattern = Jolly | Val of 'a
val most_general_match : 'a list -> 'a list -> 'a pattern list
