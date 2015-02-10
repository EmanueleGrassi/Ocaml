(* Esercizio 7 *)
(* strike_ball : 'a list -> 'a list -> int * int *)
(* strike_ball è una funzione che date due liste*)
(* restituisce una coppia contenente il numero degli elementi*)
(* uguali e nella stessa posizione ed quello degli elementi*)
(* presenti in entrambi le liste, ma in posizioni diverse*)
(* questa funzione utilizza la funzione position definita nel precedente*)
(* homework (modulo 4) ed aux*)
(* aux: (int*int) -> 'a list -> (int * int) *)
(* in questa funzione viene scandita la lista e se trovato un elemento *)
(* presente in guess viene aggiornata tmp (se l'elemento è nella stessa posizione*)
(* viene aggiornato il primo elemento della coppia tmp, altrimenti il secondo)*)
(* strike_ball_2 è la stessa funzione in cui viene fatta esplicitamente*)
(* la scansione delle due liste usando due indici(uno per lista)*)
(* usa aux*)
(* aux: 'a list -> 'a list -> (int * int) -> int -> int -> (int * int)*)
(* una funzione che prende come parametri le due liste da scandire, *)
(* temp che viene utilizzata per costruire il risultato finale e*)
(* due interi che usa come indici per scandire la lista*)

exception Not_Found
  
let position elem xs =
  let rec aux n =
    function
    | [] -> raise Not_Found
    | x :: rest -> if x = elem then n else aux (n + 1) rest
  in aux 0 xs
  
let strike_ball test guess =
  let rec aux tmp =
    function
    | [] -> tmp
    | x :: rest ->
        if List.mem x guess
        then
          if (position x test) = (position x guess)
          then aux (((fst tmp) + 1), (snd tmp)) rest
          else aux ((fst tmp), ((snd tmp) + 1)) rest
        else aux tmp rest
  in aux (0, 0) test
  
let strike_ball_2 test guess =
  let rec aux lst1 lst2 tmp m1 m2 =
    let l1 = (List.length test) - 1 in
    let l2 = (List.length guess) - 1
    in
      if m1 <= l1
      then
        if m2 <= l2
        then
          if (List.hd lst1) = (List.hd lst2)
          then
            if m1 = m2
            then
              aux lst1 (List.tl lst2) (((fst tmp) + 1), (snd tmp)) m1(m2 + 1)
            else
              aux lst1 (List.tl lst2) ((fst tmp), ((snd tmp) + 1)) m1 (m2 + 1)
          else aux lst1 (List.tl lst2) tmp m1 (m2 + 1)
        else aux (List.tl lst1) guess tmp (m1 + 1) 0
      else tmp
  in aux test guess (0, 0) 0 0
  
