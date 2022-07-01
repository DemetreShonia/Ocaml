type non_poly_tree = Empty | Node of int * non_poly_tree * non_poly_tree

let m = Node(10, Empty, Empty)

type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree

let k = Node('x', Empty, Empty)


let add_trees_with left right all =
  let add_right_tree all l =
    List.fold_left (fun a r -> Node ('x', l, r) :: a) all right in
  List.fold_left add_right_tree all left

let rec cbal_tree n =
  if n = 0 then [Empty]
  else if n mod 2 = 1 then
    let t = cbal_tree (n / 2) in
    add_trees_with t t []
  else (* n even: n-1 nodes for the left & right subtrees altogether. *)
    let t1 = cbal_tree (n / 2 - 1) in
    let t2 = cbal_tree (n / 2) in
    add_trees_with t1 t2 (add_trees_with t2 t1 []);;

cbal_tree 3;;


let rec is_mirror t1 t2 =
  match t1,t2 with
  | Empty, Empty -> true
  | Node(_, l1, r1), Node(_, l2, r2) -> is_mirror l1 r2 && is_mirror l2 r1
  | _ -> false

let is_symetric = function
  | Empty -> true
  | Node(_,l,r) -> is_mirror l r;;

let rec insert tree x = match tree with
  | Empty -> Node(x, Empty, Empty)
  | Node(y,l,r) -> if x = y then tree
  else if x < y then Node(y, insert l x, r)
  else Node (y, l, insert r x)

let construct l = List.fold_left insert Empty l;;


