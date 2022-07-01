let count_true l =
  let rec aux acc = function
  | [] -> acc
  | h::t -> if h = true then aux (acc + 1) t else aux (acc) t
  in aux 0 l;;


  count_true [true; false; true]

let build_palindrome l =
  l @ List.rev l;;

let pal = build_palindrome [1;2;3]

let is_palindrome l = l = List.rev l;;

is_palindrome pal
