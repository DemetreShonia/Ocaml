let count_true l =
  let rec aux acc = function
  | [] -> acc
  | h::t -> if h = true then aux (acc + 1) t else aux (acc) t
  in aux 0 l;;


  count_true [true; false; true]
