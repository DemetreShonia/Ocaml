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

        (* encode_type ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];; *)
      
        let decode list =
          let rec many acc n x =
            if n = 0 then acc else many (x :: acc) (n - 1) x
          in
          let rec aux acc = function
            | [] -> acc
            | One x :: t -> aux (x :: acc) t
            | Many (n, x) :: t -> aux (many acc n x) t
          in
            aux [] (List.rev list);;
        
          (* decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];; *)
        (* - : string list = *)
        (* ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] *)

      let decode lst =
        let rec many count acc x =
          if count = 0 then acc else many (count - 1) (x::acc) x
          in
          let rec aux acc = function
          | [] -> acc
          | One x :: t -> aux (x::acc) t
          | Many (n, x) :: t -> aux (many n acc x) t in
          aux [] (rev lst);;


      let encode list =
        let rle count x = if count = 0 then One x else Many (count + 1, x) in
        let rec aux count acc = function
          | [] -> [] (* Can only be reached if original list is empty *)
          | [x] -> rle count x :: acc
          | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                                  else aux 0 (rle count a :: acc) t
        in
          List.rev (aux 0 [] list);;



       let rec duplicate = function
       | [] -> []
       | h::t -> h::h::duplicate t;;

        duplicate ["a"; "b"; "c"; "c"; "d"];;
        
          let duplicate lst = (*tail recursive*)
            let rec aux acc = function
            |[] -> acc
            |h::t -> aux ((h::h::acc)) t
            in aux [] (rev lst);;

          (* duplicate ["a"; "b"; "c"; "c"; "d"];; *)
          (* - : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"] *)


          let replicate list n =
            let rec prepend n acc x =
              if n = 0 then acc else prepend (n-1) (x :: acc) x in
            let rec aux acc = function
              | [] -> acc
              | h :: t -> aux (prepend n acc h) t in
            aux [] (List.rev list);;
        
          (* replicate ["a"; "b"; "c"] 3;; *)

          let drop lst n =
            let rec aux acc count = function
            | [] -> acc
            | h::t -> if count = 1 then aux acc n t
            else aux (h::acc) (count -1) t
            in aux [] (n-1) (rev lst);;

            (* drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;; *)
          
 (* this splits as n,n,n,n,n *)
            let split lst n =
              let rec aux current acc count = function
              | [] -> acc
              | h::t -> if count = 1 then aux [] ((h::current):: acc) n t
              else aux (h::current) acc (count-1) t
              in aux [] [] n (rev lst);;  


            let split list n =
              let rec aux i acc = function
                | [] -> List.rev acc, []
                | h :: t as l -> if i = 0 then List.rev acc, l
                                  else aux (i - 1) (h :: acc) t 
              in
                aux n [] list;;


            let split lst n =
              let rec aux count acc = function
              | [] -> (rev acc, [])
              | h::t as l -> if count = 0 then (rev acc, l)
              else aux (count -1) (h::acc) t in aux n [] lst;;

            (* split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;; *)
            (* - : string list * string list = *)
            (* (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]) *)
            (* # split ["a"; "b"; "c"; "d"] 5;; *)
            (* - : string list * string list = (["a"; "b"; "c"; "d"], []) *)


let slice lst a b =
  let rec aux count acc = function
  | [] -> rev acc
  | h::t -> 
  if count < a then aux (count + 1) acc t
  else if count = b then rev (h::acc)
  else aux (count + 1) (h::acc) t
  in aux 0 [] (lst);;


  let slice list i k =
    let rec take n = function
      | [] -> []
      | h :: t -> if n = 0 then [] else h :: take (n - 1) t
    in
    let rec drop n = function
      | [] -> []
      | h :: t as l -> if n = 0 then l else drop (n - 1) t
    in
    take (k - i + 1) (drop i list);;

(* slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;; *)
(* - : string list = ["c"; "d"; "e"; "f"; "g"] *)


let rotate lst n = 
  let k = n mod List.length(lst) in
  let rec aux count acc = function
  | [] -> []
  | h::t as m -> if count = 0 then m@(rev acc)
  else aux (count - 1) (h::acc) t
  in aux k [] (lst);;

  (* rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 50;; *)


  (* tail recursive *)
let remove_at id lst = 
  let rec aux count acc = function
  | [] -> rev acc
  | h::t  -> if count = 0 then aux (count -1 ) acc t
  else aux (count - 1) (h::acc) t in
  aux id []  (lst);; 

  let rec remove_at n = function
  | [] -> []
  | h::t -> if n = 0 then t
  else h:: remove_at (n-1) t;;


  (* remove_at 0 ["a"; "b"; "c"; "d"];; *)
  (* - : string list = ["a"; "c"; "d"] *)


let rec insert_at x n = function
| [] -> [x]
| h::t as l -> if n = 0 then x::l else h::insert_at x (n-1) t;;
  (* insert_at "alfa" 2 ["a"; "b"; "c"; "d"];; *)
  (* - : string list = ["a"; "alfa"; "b"; "c"; "d"] *)



let range a b =
let rec aux a b counter lst = if counter = b then rev (b::lst)
else aux a b (counter + 1) (counter::lst) in aux a b a [];;

(* range 3 5;; *)

let rand_select list n =
  let rec extract acc n = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then (h, acc @ t) else extract (h :: acc) (n - 1) t
  in
  let extract_rand list len =
    extract [] (Random.int len) list
  in
  let rec aux n acc list len =
    if n = 0 then acc else
      let picked, rest = extract_rand list len in
      aux (n - 1) (picked :: acc) rest (len - 1)
  in
  let len = List.length list in
    aux (min n len) [] list len;;

    rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 1;;
  
  
(* # range 4 9;; *)
(* - : int list = [4; 5; 6; 7; 8; 9] *)

(* let lotto_select n m = rand_select (range 1 m) n;; *)


(* lotto_select 6 49;; *)

(* we have to skip some *)

let gray n =
  let rec gray_next_level k l =
    if k < n then
      (* This is the core part of the Gray code construction.
       * first_half is reversed and has a "0" attached to every element.
       * Second part is reversed (it must be reversed for correct gray code).
       * Every element has "1" attached to the front.*)
      let (first_half,second_half) =
        List.fold_left (fun (acc1,acc2) x ->
            (("0" ^ x) :: acc1, ("1" ^ x) :: acc2)) ([], []) l
      in
      (* List.rev_append turns first_half around and attaches it to second_half.
       * The result is the modified first_half in correct order attached to
       * the second_half modified in reversed order.*)
      gray_next_level (k + 1) (List.rev_append first_half second_half)
    else l
  in
    gray_next_level 1 ["0"; "1"];;


    (* gray 3;; *)


    List.mapi (fun i x -> x * i) [1;2;5;6;7;8;3;0;213];;
    List.fold_left (fun x y -> x-y) 0 [2;2;2;2];;
    List.fold_right (fun x y -> x-y) [2;2;2;2] 0;;