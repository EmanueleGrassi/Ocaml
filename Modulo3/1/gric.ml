(* Esercizio 1 *)
(* *)
exception Orario_malformato
  
let somma_ore (h1, m1) (h2, m2) =
  let verifica_orario (h, m) = ((h < 24) && (h >= 0)) && ((m < 60) && (m > 0))
  in
    if (not (verifica_orario (h1, m1))) || (not (verifica_orario (h2, m2)))
    then raise Orario_malformato
    else
			begin
         let sumh = h1 + h2 in
         let summ = m1 + m2
           in
             if sumh < 23
             then ((sumh + (summ / 60)), (summ mod 60))
             else (((sumh + (summ / 60)) - 24), (summ mod 60))
			end
  
