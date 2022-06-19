let rec last = function 
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t;;

 let rec last_two = function
  | [] | [_] -> None
  | [x; y] -> Some (x,y)
  | _ :: t -> last_two t;;

  let length list =
    let rec aux n = function
      | [] -> n
      | _ :: t -> aux (n + 1) t
    in
    aux 0 list;;

  let rec at k = function
    | [] -> None
    | h :: t -> if k = 0 then Some h else at (k - 1) t;;


  let rev l = 
    let rec aux acc = function
    | [] -> acc
    | h::t -> aux (h::acc) t in
    aux [] l;;

  (* rev [1;2;3];; *)

  let is_palindrome lst = lst = rev lst;; (*or List.rev*)
    (* palindrome is its reverse *)

  (* is_palindrome [1;1];; *)

  type 'a node =
    | One of 'a 
    | Many of 'a node list

let flatten lst = 
  let rec aux acc = function
  | [] -> acc
  | (One x) :: xs -> aux (x::acc) xs
  | (Many x) :: xs -> aux (aux acc x) xs   
  in rev (aux [] lst);;

  (* flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];; *)



let rec compress = function
| x :: (y::_ as t) -> if x = y then compress t else x :: compress t
| last -> last;;

  (* compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];; *)
  (* - : string list = ["a"; "b"; "c"; "a"; "d"; "e"] *)


    let pack lst =
      let rec aux current acc = function
      | [] -> []
      | [x] -> (x::current) :: acc
      | a::(b::_ as t) -> if a = b then aux (a::current) acc t
      else aux [] ((a::current) :: acc) t
      in aux [] [] lst;;

    (* pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];; *)
    (* - : string list list = *)
    (* [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; *)
     (* ["e"; "e"; "e"; "e"]] *)


      let encode lst =
        let rec aux count acc = function
        | [] -> []
        | [x] -> (count + 1, x) :: acc
        | a::(b::_ as t) -> if a = b then aux (count + 1) acc t
        else aux 0 ((count + 1, a)::acc) t
        in aux 0 [] lst;; 


      (* encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];; *)
     (* - : (int * string) list = *)
     (* [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] *)


     type 'a rle =
      | One of 'a
      | Many of int * 'a;;
    type 'a rle = One of 'a | Many of int * 'a
    
    let encode_type lst =
      let create_tuple count elem =
        if count = 1 then One elem
        else Many (count,elem) in
        let rec aux count acc = function
        |[] -> []
        | [x] -> (create_tuple (count + 1) x)::acc
        | a::(b::_ as t) -> if a = b then aux (count + 1) acc t
        else aux 0 ((create_tuple (count + 1) a) :: acc) t
        in rev (aux 0 [] lst);;

        encode_type ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
      