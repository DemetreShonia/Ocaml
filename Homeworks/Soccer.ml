type 'a result = Win | Draw | Loss 
(* let rec series g p =
  if g = 0 then (if p = 0 then [[]]) else [])
  else map (fun r -> Win::r) (series(g-1) (p-3))
  @ map (fun r -> Loss::r) (series(g-1) (p-2))
  @ map (fun r -> Draw::r) (series(g-1) p) *)

let series_last g p = 
  let rec aux g p s acc =
    if g = 0 then (if p = 0 then s::acc else acc)
    else aux (g-1) p (Loss::s) acc 
    |> aux (g-1) (p-1) (Draw::s)|>
    aux (g-1) (p-3) (Win::s) in aux g p [] [];;

    series_last 3 4;;
