exception Failure of string

let split n l =
  if n < 0 then raise(Failure("The number can not be negative"))
  else let rec aux acc n = function
  | [] -> (List.rev(acc), [])
  | h::t -> if (n = 0) then (List.rev(acc), t) else aux (h::acc) (n-1) t
  in aux [] n l;;


  (* exception Failure of string *)

(* split ((2), []);; *)

