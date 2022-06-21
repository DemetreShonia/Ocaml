(* let rec check(funct, v, l) =
  match l with
 | [] -> false
 | h::t -> if funct(v, h) then true else check(funct, v, t) *)

 let rec check f a b = match b with
 | [] -> false
 | h::t -> if f (h, a) then true else check f a t;;

 (* MAX? *)