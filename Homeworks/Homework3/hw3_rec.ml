(* optional recursion 90%*)

(* 29.7*)
let rec leftover_count lst counter : int=
  match (lst, counter) with
  | ([], counter) -> counter
  | (lst, 0) -> 
          if (lst <> []) then leftover_count lst (List.hd lst)
          else List.hd lst (* get the list.hd and return as int*)
  | (lst, counter) ->
      match (lst, counter) with
      | ([], counter) -> counter
      | (lst, 0) -> leftover_count lst (List.hd lst)
      | (h :: t, counter) -> 
          if counter != 0 then leftover_count t (counter-1)
          else leftover_count t (counter -1)  

let test_leftover_count () = 
  assert (leftover_count [] 1 = 1);
  assert (leftover_count [1;4] 2 = 0);
  assert (leftover_count [1;4] 5= 3);
  assert (leftover_count [1;4;2;10;1;4;2;10] 2= 1);
  assert (leftover_count [1;4;2;10;3;5;7;11] 3= 5);
  assert (leftover_count [1;4;2;10;3;5;7;11] 5= 2 );
  assert (leftover_count [1;4;2;10;3;5;7;11] 7= 10 );
  print_endline "All leftover_count tests passed!" 

let run_all_tests () = 
  test_leftover_count ();
  print_endline "All tests passed!" 

  let _ = run_all_tests ()