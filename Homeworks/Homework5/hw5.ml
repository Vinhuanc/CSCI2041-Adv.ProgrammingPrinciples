(* Received 99/100 points.*)
(* Failed this case: Check `Tree_ops.is_tree_sorted (Node (-1, Leaf, Leaf))`. (0/1) *)
type tree =
  | Leaf
  | Node of int * tree * tree

let with_default default op = 
  match op with
  | None -> default
  | Some x -> x

module Tree_ops =
struct 

  let rec sum t =
    match t with
    | Leaf -> 0
    | Node(n, l, r) -> n + sum l + sum r

  let rec tmax t : int option =
    match t with
    | Leaf -> None
    | Node (v, l, r) ->
      let max_left = tmax l in
      let max_right = tmax r in
      let find_max x y =
        match (x, y) with
        | (Some a, Some b) -> Some (max a b)
        | (Some a, None) -> Some a
        | (None, Some b) -> Some b
        | (None, None) -> None
      in
      find_max (Some v) (find_max max_left max_right)

      let rec tmin t : int option =
        match t with
        | Leaf -> None
        | Node (v, l, r) ->
          let max_left = tmin l in
          let max_right = tmin r in
          let find_min x y =
            match (x, y) with
            | (Some a, Some b) -> Some (min a b)
            | (Some a, None) -> Some a
            | (None, Some b) -> Some b
            | (None, None) -> None
          in
          find_min (Some v) (find_min max_left max_right)

      let return_int x : int = 
        match x with 
        | None -> 0
        | Some a -> a

   let rec flatten t :int list=
   match t with
   | Leaf -> []
   | Node(n, l, r) -> flatten l @ [n] @ flatten r


   (* (-1, Leaf, Leaf) *)
 let rec is_tree_sorted t :bool=
  let tmax_tree = tmax t in
   match t with
      | Leaf -> true
      | Node(x, l, r) -> 
        let tmax_tree_l = tmax l in (* true*)
        let tmin_tree_r = tmin r in(* true*)
        
        if (return_int (tmax_tree_l )<= x && x <= return_int (tmin_tree_r)) then true 
        else if ( l=Leaf && r=Leaf) then true
        else false

end


let test_with_default () = 
  assert (with_default (-1) None = -1);
  assert (with_default (-1) (Some 7) = 7);
  print_endline "All with_default tests passed!" 

let test_tree_sum ()=
  assert (Tree_ops.sum Leaf = 0);
  assert (Tree_ops.sum (Node (7, Node (8, Leaf, Leaf), Node (-9, Leaf, Leaf))) = 6);
  print_endline "All sum tests passed!" 

let test_tmax ()=
assert(Tree_ops.tmax Leaf = None);
assert(Tree_ops.tmax (Node (7, Node (8, Leaf, Leaf), Node (-9, Leaf, Leaf))) = Some 8);
print_endline "All tmax tests passed!" 

let test_flatten () = 
  assert(Tree_ops.flatten Leaf = []);
  assert(Tree_ops.flatten (Node (7, Node (8, Leaf, Leaf), Node (-9, Leaf, Leaf))) = [8; 7; -9]);
  assert(Tree_ops.flatten (Node (1, Node (21, Node (31, Leaf, Leaf), Node (32, Leaf, Leaf)), Node (22, Node (33, Leaf, Leaf), Node (34, Leaf,
  Leaf)))) = [31; 21; 32; 1; 33; 22; 34]);
print_endline "All flatten tests passed!" 

let test_sorted() =
assert(Tree_ops.is_tree_sorted Leaf = true);
assert(Tree_ops.is_tree_sorted (Node (7, Node (8, Leaf, Leaf), Node (-9, Leaf, Leaf))) = false);
assert(Tree_ops.is_tree_sorted (Node (2, Node (2, Leaf, Leaf), Node (2, Leaf, Leaf))) = true);
assert(Tree_ops.is_tree_sorted (Node (35, Node (12, Node (7, Leaf, Leaf), Node (35, Leaf, Leaf)), Node (48, Leaf, Leaf))) = true);
assert(Tree_ops.is_tree_sorted (Node (35, Node (12, Node (7, Leaf, Leaf), Node (35, Leaf, Leaf)), Node (48, Leaf, Leaf))) = true);
assert(Tree_ops.is_tree_sorted (Node (-1, Leaf, Leaf))=true);
print_endline "All sorted tests passed!" 

let run_all_tests () = 
  test_with_default  ();
  test_tree_sum ();
  test_tmax ();
  test_flatten ();
  test_sorted ();
  print_endline "All tests passed!" 

  let _ = run_all_tests ()