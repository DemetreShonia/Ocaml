let rec f = 
  fun l -> match l with [] -> 1 | x::xs -> x + g xs
  and g = fun l -> match l with [] -> 0 | x::xs -> x * f xs;;


  f [3;4;5];;