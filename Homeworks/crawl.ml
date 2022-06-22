type tree = Empty 
          | Node of int * tree * tree
type command = Left | Right | Up | New of int | Delete | Push | Pop
 
(* print a graphical representation (dot) of a binary tree (2. argument) to a file (1. argument) *)
let print_tree filename btree = 
  let file = open_out filename in
  Printf.fprintf file "digraph Tree {\n";
  let rec print next_id = function Empty -> 
    Printf.fprintf file "\tn%d[shape=rectangle,label=\"\"];\n" next_id; next_id + 1, next_id
  | Node (x, l, r) ->
    let node_id = next_id in
    Printf.fprintf file "\tn%d[label=\"%d\"];\n" node_id x;
    let next_id, lid = print (next_id + 1) l in
    let next_id, rid = print next_id r in 
    (Printf.fprintf file "\tn%d -> n%d[label=\"L\"];\n" node_id lid);
    (Printf.fprintf file "\tn%d -> n%d[label=\"R\"];\n" node_id rid);
    next_id, node_id
  in
  ignore(print 0 btree);
  Printf.fprintf file "}";
  close_out file
 
let crawl cmds tree =
let rec crawl_helper c t root stack =
match c,t with
| [],_ -> (match root with 
            | [] -> t
            | (Node(v,l,r), lOr)::rest -> (match lOr with
                                               | 1 -> crawl_helper c (Node(v,t,r)) rest stack   
                                               | 2 -> crawl_helper c (Node(v,l,t)) rest stack
                                               | _ ->  crawl_helper c (Node(v,l,r)) rest stack
                                               )
            | _ -> raise(Failure "fail"))
| (Left)::cml, (Node (v,l,r)) -> crawl_helper cml l ((t,1)::root) stack 
| (Left)::cml, Empty -> raise(Failure "no child")
| (Right)::cml, (Node (v,l,r)) -> crawl_helper cml r ((t,2)::root) stack 
| (Right)::cml, Empty -> raise(Failure "no child")
| (Delete)::cml, _ -> crawl_helper cml Empty root stack 
| (New x)::cml, _-> crawl_helper cml (Node(x,Empty,Empty)) root stack 
| (Push)::cml, _ -> crawl_helper cml t root (t::stack) 
| (Pop)::cml, _ -> (match stack with 
                         | [] -> raise(Failure "no stack")
                         | s::st -> crawl_helper cml s root st 
                         )
| (Up)::cml, _ -> (match root with 
            | [] -> raise(Failure "no parent")
            | (Node(v,l,r),lOr)::rest -> (match lOr with
                                               | 1 -> crawl_helper cml (Node(v,t,r)) rest stack   
                                               | 2 -> crawl_helper cml (Node(v,l,t)) rest stack
                                               | _ ->  crawl_helper cml (Node(v,l,r)) rest stack
                                               )
            | _ -> raise(Failure "fail"))
in crawl_helper cmds tree [] [];;
 
let t_l = Node (2, Node (1, Empty, Empty), Node (3, Empty, Empty))
let t_r = Node (6, Node (5, Empty, Empty), Node (7, Empty, Empty))
let tree = Node (4, t_l , t_r);;
crawl [Left; Right; Up; Left; Up; Up; New 3] tree;;
crawl [New 555;Delete] tree;;
 
 