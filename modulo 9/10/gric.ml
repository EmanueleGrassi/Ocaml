(* Esercizio 10 *)
(* dnf: form -> form*)
(* La funzione dnf trasforma una formula in forma normale digiuntiva.*)
(* dnf utilizza fnn ed aux*)
(* fnn: form -> form*)
(* La funzione nnf trasforma una formula in forma normale negativa *)
(* aux: form -> form*)
(* questa funzione effettua la trasformazione della formula in forma normale*)
(* disgiuntiva, ma ha bisogno di una formula normale negativa come parametro*)

type form =
  | True
  | False
  | Prop of string
  | Not of form
  | And of form * form
  | Or of form * form
  | Imp of form * form

let rec fnn = function
  | And(f,g) -> And(fnn f,fnn g)
  | Or(f,g) -> Or(fnn f,fnn g)
  | Imp(f,g) -> fnn(Or(Not f,g))
  | Not(And(f,g)) -> Or(fnn(Not f),fnn(Not g))
  | Not(Or(f,g))  -> And(fnn(Not f),fnn(Not g))
  | Not(Imp(f,g)) -> And(fnn f,fnn(Not g))
  | Not(Not f) -> fnn f
  | f -> f

let rec dnf f =
  let rec aux = function
    | And(f, g) ->
        (match ((aux f), (aux g)) with
         | (And(f1,g1),h) | (h,And(f1,g1)) -> aux (Or((And(h,f1)),(And(h,g1))))
         | (f1,h) -> And(f1,h))
    | Or(f,g) -> Or((aux f),(aux g))
    | f -> f
  in aux (fnn f)
  
	
let rec mkand = function
    [] -> True
  | [f] -> f
  | f::rest -> And(f,mkand rest)	
	
let rec mkdnf = function
    [] -> False (* se non ci sono rami aperti, la formula
                   e' contraddittoria *)
  | [branch] -> mkand branch
  | branch::rest ->
      Or(mkand branch,mkdnf rest)

let complement = function
    Prop p -> Not(Prop p)
  | Not(Prop p) -> Prop p
  | _ -> failwith "complement"

let alpha = function
    And(f,g) -> (f,g)
  | Not(Or(f,g)) -> (Not f,Not g)
  | Not(Imp(f,g)) -> (f,Not g)
  | _ -> failwith "alpha"

(* val beta : form -> form * form *)
let beta = function
    Or(f,g) -> (f,g)
  | Not(And(f,g)) -> (Not f,Not g)
  | Imp(f,g) -> (Not f,g)
  | _ -> failwith "beta"

let all_models formlist =
  let rec aux pending lits = 
    match pending with
      [] -> [lits]
    | f::rest ->
        match f with
          True | Not False -> aux rest lits
        | False | Not True -> []
        | Prop _ | Not(Prop _) ->
            if List.mem f lits then aux rest lits
            else if List.mem (complement f) lits  then []
            else aux rest (f::lits)
        | Not(Not f) -> aux (f::rest) lits
	| And(_,_) | Not(Or(_,_)) | Not(Imp(_,_)) ->
	    let (f1,f2)= alpha f in
	    aux (f1::f2::rest) lits
	| Or(_,_) | Not(And(_,_)) | Imp(_,_) -> 
	    let (f1,f2) = beta f in
            (aux (f1::rest) lits) @ (aux (f2::rest) lits)
   in aux formlist []

(* dnf_tab: form -> form *)
let dnf_tab f =
  mkdnf(all_models [f])
	
	