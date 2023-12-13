let is_some x = match x with
  | Some _ -> true
  | _ -> false

let rec get_somes xs = match xs with
  | [] -> []
  | Some x :: xs -> x :: get_somes xs
  | None :: xs -> get_somes xs

let rec length lst = match lst with
  | [] -> 0
  | _::xs -> 1 + length xs

module type OptionFunction = sig (* signature *)
  type input
  type output
  val fn : input -> output option
  (** The domain fn should predict whether it yields Some or not.
      That is: it should satisfy the following:
      [domain x = is_some (fn x)]
      (The above would be a valid implementation of domain,
       but there might be a more efficient implementation) **)
  val domain : input -> bool
end

(** Finding the max integer, returns None if no such number exists **)
module FindMax = struct (* module *)
  type input = int list
  type output = int
  (* Note: this operation is O(n) in the input *)
  let rec fn lst = match lst with
    | [] -> None
    | x::xs -> (match fn xs with
                | None -> Some x
                | Some v -> Some (if x > v then x else v))
  (* Note: this operation is O(1) in the input *)
  let domain lst = match lst with
    | [] -> false
    | _ -> true
end

module SolveList(F : OptionFunction) = struct
  let rec solve lst = match lst with
    | [] -> []
    | x::xs -> (match F.fn x with
                | None -> solve xs
                | Some v -> v :: solve xs)
  let rec filter lst = match lst with
    | [] -> []
    | x::xs -> if F.domain x
               then x :: filter xs
               else filter xs
  (** just apply the function, leaving the None in place **)
  let rec map lst = match lst with
    | [] -> []
    | x::xs -> F.fn x :: map xs
end

(* 
   P1. prove that FindMax satisfies the invariants of OptionFunction
       (uses induction on the list)

   P2. prove that get_somes (map lst) = solve lst
       (induction on the list, doesn't require properties of OptionFunction)

   P3. prove that solve (filter lst) = solve lst
       (induction on the list, requires properties of OptionFunction)

   P4. prove that length (solve lst) = length (filter lst)
       (induction on the list,
        either requires properties of OptionFunction or the use of P3,
        doesn't require properties of 0, 1 and +)

   *)