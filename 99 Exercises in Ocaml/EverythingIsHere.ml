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