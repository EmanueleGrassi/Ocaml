(* Esercizio 9 *)
(* int2form: (string * bool) list -> form *)
(* è una funzione che data un'interpretazione, genera la formula rappresentata da essa*)
(* Questa funzione solleva l'eccezione Interpretazione_vuota se viene passata come parametro*)
(* un'interpretazione vuota*)

type interpretation = (string * bool) list

exception Interpretazione_vuota

let rec int2form = function
	| [] -> raise Interpretazione_vuota
	| [(x,y)] -> (match y with
    	          | true -> Prop x
    						| false -> Not (Prop x) )
	| (x,y)::rest -> (match y with
        	          | true -> And(Prop x, int2form rest)
        						| false -> And(Not (Prop x), int2form rest ))