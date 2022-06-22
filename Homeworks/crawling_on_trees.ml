module Crawler = struct
  type tree = Empty | Node of int * tree * tree
  type command = Left | Right | Up | New of int | Delete | Push | Pop

(*Left moves the crawler to the current node’s left child.
Right moves the crawler to the current node’s right child.
Up moves the crawler up to the current node’s parent node.
New x replaces the current node (including all children) with a new node with value x.
Delete removes the current node (including all children) leaving behind an Empty- leaf.
Push pushes the subtree rooted at the current node onto the stack. The tree stays unchanged.
Pop replaces the subtree rooted at the current node with the topmost tree of the stack. The tree is then popped from the stack.*)
  let crawl cmd_list tr =
    let rec aux cmd_list tr way stack =
      match cmd_list with
      | [] -> tr
      | current_command::t ->
      (
        match current_command with
        | Left ->begin
          match tr with
          | Empty->Empty
          | Node(v,lt,rt) -> aux t lt way stack
        end 
        | Right -> begin
          match tr with
          | Empty->Empty
          | Node(v,lt,rt) -> aux t rt way stack
        end
        | Up -> let way_to_here = List.rev(way) in begin
          match way_to_here with
          | [] -> Empty (*Or what?*)
          | h::t -> aux t h (List.rev(t)) stack   
        end
        | New x -> aux t (Node(x, Empty, Empty)) way stack
        | Delete -> Empty (*IDK THIS ONE YET*)
        | Push -> aux t tr way (tr::stack) (*Saves current tree in stack, pushes it where it is now!*)
        | Pop -> begin
          match stack with
          | [] -> aux t tr way stack
          | h::_ -> aux t h way stack
      )

      
  let t_l = Node (2, Node (1, Empty, Empty), Node (3, Empty, Empty))
  let t_r = Node (6, Node (5, Empty, Empty), Node (7, Empty, Empty))
  let tree = Node (4, t_l , t_r);;

  crawl [Left; Right; Up; Left; Up; Up; New 3] tree;;
end



