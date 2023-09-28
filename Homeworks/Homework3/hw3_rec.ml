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

(* let list_size_bool lst x : bool=
  if x > List.length lst then true
  else false *)

let rec last_card lst c : int = (* counter reset = last index.   counter > list.length -> -1*)
match (lst, c) with
  
  | ([], c) -> c
  | (lst, 0) -> List.hd lst
  (* | (lst, c) -> if List.length lst < c then -1 else last_card lst (c-1) *)
  | (h :: t, c) -> if List.length lst < c then -1
                 else if (c != 0) then last_card t (c-1)
                 else if (c < List.length lst) then h
                 else
                  (* if (c=1) then *)
                     h
               

   

let run_last_card_test () =
  assert(last_card [1;4] 5 = -1);
  assert(last_card [1;4] 1 = 4);
  (* assert(last_card [1;4;2;10;3;5;7;11] 1 = 5); *)
  assert(last_card [1;4;2;10;3;5;7;11] 3 = 10);
  assert(last_card [1;4;2;10;3;5;7;11] 7 = 11);
  print_endline "All run_last_card_test passed!" 



(* let test_leftover_count () = 
  assert (leftover_count [] 1 = 1);
  assert (leftover_count [1;4] 2 = 0);
  assert (leftover_count [1;4] 5= 3);
  assert (leftover_count [1;4;2;10;1;4;2;10] 2= 1);
  assert (leftover_count [1;4;2;10;3;5;7;11] 3= 5);
  assert (leftover_count [1;4;2;10;3;5;7;11] 5= 2 );
  assert (leftover_count [1;4;2;10;3;5;7;11] 7= 10 );
  print_endline "All leftover_count tests passed!"  *)


let run_all_tests () = 
  (* test_leftover_count (); *)
  run_last_card_test ();
  print_endline "All tests passed!" 

  let _ = run_all_tests ()
