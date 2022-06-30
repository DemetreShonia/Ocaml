let rec fl f a l = match l with [] -> a
| x::xs -> fl f (f a x) xs
let rec fr f l a = match l with [] -> a
| x::xs -> f x (fr f xs a)
let rec rev_map f l a = match l with [] -> a
| x::xs -> rev_map f xs (f x :: a)
let (+) a b = a + b


(* fl (+) 0 (rev_map (fun x -> x * 2) l []) = fr (fun x a -> a + 2 * x) l 0 *)



fl (+) 0 (rev_map (fun x -> x * 2) [1;2;3;4] [])

