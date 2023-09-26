(* Example *)
let parenthesis_check_rec (lst: string list) : int option =
	let rec helper lst accu = 
		match lst with
		| [] -> accu
		| h :: t -> helper t (
			match h, accu with 
			| ")", Some x -> if (x - 1) < 0 then None else Some (x - 1)
			| "(", Some x -> Some (x + 1)
			| _, y -> y
		)
	in
	helper lst (Some 0)

(* Example *)
let parenthesis_check_fold (lst: string list) : int option =
	List.fold_left (
		fun accu h -> 
			match h, accu with 
			| ")", Some x -> if (x - 1) < 0 then None else Some (x - 1)
			| "(", Some x -> Some (x + 1)
			| _, y -> y
		) (Some 0) lst 

let rec marco_polo_rec (lst: string list) : bool option =
    let rec helper lst p =
        match lst with
        | [] -> if p then Some true else Some false
        | "marco" :: t -> if p then None else helper t true
        | "polo" :: t -> if p then helper t false else None
        | _ :: t -> helper t p
    in
    helper lst false

let marco_polo_fold (lst: string list) : bool option =
	List.fold_left( fun acc h ->
        match acc, h with
        | None, _ -> None
        | _, "marco" -> if acc = Some true then None else Some true
        | _, "polo" -> if acc = Some true then Some false else None
        | acc, _ -> acc
    ) (Some false) lst

let combine_fold (lst: string list) : int option * bool option = 
	let cf ( x, y) h =
        (match x, h with
            | Some e1, ")" -> if (e1-1)<0 then None else Some(e1-1)
            | Some e1, "(" -> Some(e1+1)
            | y, _ -> y
        ),
        (match y, h with
            | _, "marco" -> if y = Some true then None else Some true
            | _, "polo" -> if y = Some true then Some false else None
            | acc, _ -> acc)
    in List.fold_left cf ( Some 0, Some false) lst
(* TODO *)
let combine_rec (lst: string list) : int option * bool option = 
	parenthesis_check_rec lst, marco_polo_rec lst

(* Tests for marco_polo_rec *)
let test_marco_polo_rec () =
    assert (marco_polo_rec ["marco"; "polo"; "marco"; "polo"] = Some false);
    assert (marco_polo_rec ["marco"; "marco"] = None);
    assert (marco_polo_rec ["polo"; "polo"] = None);
    print_endline "All tests for marco_polo_rec passed!"

(* Tests for marco_polo_fold *)
let test_marco_polo_fold () =
    assert (marco_polo_fold ["marco"; "polo"; "marco"; "polo"] = Some false);
    assert (marco_polo_fold ["marco"; "marco"] = None);
    assert (marco_polo_fold ["polo"; "polo"] = None);
    print_endline "All tests for marco_polo_fold passed!" 

(* Tests for combine_fold *)
(* Tests for combine_fold *)
let test_combine_fold () =
    assert (combine_fold ["("; ")"; "marco"; "polo"] = (Some 0, Some false));
    assert (combine_fold [")"; ")"; "marco"; "marco"] = (None, None));
    assert (combine_fold ["("; "("; "polo"; "polo"] = (Some 2, None));
    assert (combine_fold ["("; ""; ")"] = (Some 0, Some false));
    print_endline "All tests for combine_fold passed!"

(* Tests for combine_rec *)
let test_combine_rec () =
    assert (combine_fold ["("; ")"; "marco"; "polo"] = (Some 0, Some false));
    assert (combine_fold [")"; ")"; "marco"; "marco"] = (None, None));
    assert (combine_fold ["("; "("; "polo"; "polo"] = (Some 2, None));
    assert (combine_fold ["("; ""; ")"] = (Some 0, Some false));
    print_endline "All tests for combine_rec passed!" 


let run_all_tests () =
    test_marco_polo_rec ();
     test_marco_polo_fold ();
    test_combine_fold ();
    test_combine_rec ();
    
    print_endline "All tests passed!" 

(* UNCOMMENT THE BELOW LINE TO RUN TESTS *)

let _ = run_all_tests ()

