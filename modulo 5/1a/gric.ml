(* Esercizio 1a *)
let in_labirinto dim (r, c) =
  (r >= 0) && ((c >= 0) && ((r < dim) && (c < dim)))
  
let rec find_content contenuti c =
  match contenuti with
  | [] -> []
  | (x, caselle) :: rest ->
      if List.mem c caselle
      then x :: (find_content rest c)
      else find_content rest c
  
let filter_vicini dim lst =
  let rec aux tmp =
    function
    | [] -> List.rev tmp
    | casella :: rest ->
        if in_labirinto dim casella
        then aux (casella :: tmp) rest
        else aux tmp rest
  in aux [] lst
  
let raccolti contenuti lst =
  let rec aux tmp =
    function
    | [] -> tmp
    | casella :: rest ->
        aux (List.append (find_content contenuti casella) tmp) rest
  in aux [] lst
  
