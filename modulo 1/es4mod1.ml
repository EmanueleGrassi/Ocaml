(*
ESERCIZIO 4 GRUPPO 1
*) 

(* pi1 : 'a * 'b * 'c * 'd -> 'a *)
(* lo stesso ragionamento vale per le altre 3 funzioni*)
(* queste funzioni non possono essere applicate ad una quintupla*)
(* perchè questa espressione si aspetta una quadrupla come parametro formale *)
let pi1 (x,y,w,z) = x;;
let pi2 (x,y,w,z) = y;;
let pi3 (x,y,w,z) = w;;
let pi4 (x,y,w,z) = z;;

(* pi3 (pi2 quadrupla) ha valore unit   *)
(* pi4 (pi2 quadrupla) ha valore 1   *)