module type ProperS =
sig
	val plus : int -> int -> int option
	val divide : int -> int -> int option
	val subtract : int -> int -> int option
end

(* Write the functions defined in the above signature 
	Note: these functions are the same as the ones given
	in previous labs and lectures *)
module Proper : ProperS = struct
	let ( >>= ) (opt: 'a option) (f: ('a -> 'b option)) : 'b option =
    match opt with
    | Some x -> f x
    | None -> None
	(* let plus ... *)
	let plus x y = 
		let result = x + y   in
		match (x>=0,y>=0, result>=0) with
		| (true, true, true) -> Some result
    | (false, false, false)
    | (true, false, _)
    | (false, true, _) -> Some result  
    | (_, _, _) -> None

	(* let divide ... *)
	let divide x y =
		match x, y with
	| _ when x = min_int && y = (-1) -> None
	| _, 0 -> None
	| _ -> Some (x / y)
	(*
	   Create the negate function:
	   type: int -> int option
	   note: this function returns None if the input equals min_int 
	*)
	(* let negate ... *)
	let negate x = 
		if x=min_int then None else Some (-x)

	(* use negate then plus in the subtract function *)
	(* let subtract ... *)
	let subtract x y =  
		 (Some y)>>=negate >>=plus x
		
end


(* When you are finished writing the functions, 
	uncomment the below line. Then remove multiply 
	from the signature and try again *)
module Test = (Proper: ProperS)

(* TESTS *)
let _  = Test.subtract 3 1;

