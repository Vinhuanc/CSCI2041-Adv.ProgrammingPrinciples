(* optional recursion 90% *)
(* let rec leftover_count lst counter : int=
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

let rec last_card lst counter : int = 
    match (lst, counter) with
    |([], counter) -> counter
    |(lst, 0) -> if(lst <>[] && List.length lst > 0) then last_card lst (List.hd lst) 
    else 0 
    |((h:: t), counter) ->
      if (counter > List.length t && counter=0) then -1
      else if(counter > List.length t && counter!=0 && List.length t ==1 ) then -1
      else if (counter > List.length t && counter!=0 ) then h 
      else if(counter > List.length t && h<counter) then 242
      else last_card t (counter -1)
 
let helper lst sum  : int =
  let (x, y) = List.fold_left (
  fun (sum, counter) h ->
  if sum > h then (sum + h, counter) else (sum + h, counter + 1) ) 
  (sum, 0) lst in y

let num_greater_than_sum (lst : int list) : int =
     helper lst 0
*)



let leftover_count lst x : int =
  List.fold_left ( (* val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc *)
    fun counter h ->
      match counter with
      | 0 -> h -1 (* essentially move to the List's next iteration*)
      | num -> counter - 1
  ) x lst (* val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc *)

let last_card lst x = 
  let (counter, last) = List.fold_left(
    fun(counter, last) h ->
      match counter with 
      | 0 -> (h-1, h)
      | num -> (counter-1, last)
  ) (x, -1) lst in last

let num_greater_than_sum lst = 
  let (x, y) = List.fold_left (
     fun (x, y) h ->
  if h > x then (x + h, y+1) else (x+h, y))
  (0, 0) lst in y

  let test_leftover_count () = 
    assert (leftover_count [] 1 = 1);
    assert (leftover_count [1;4] 2 = 0);
    assert (leftover_count [1;4] 5= 3);
    assert (leftover_count [1;4;2;10;1;4;2;10] 2= 1);
    assert (leftover_count [1;4;2;10;3;5;7;11] 3= 5);
    assert (leftover_count [1;4;2;10;3;5;7;11] 5= 2 );
    assert (leftover_count [1;4;2;10;3;5;7;11] 7= 10 );
    print_endline "All leftover_count tests passed!" 
  let test_last_card_test () =
    assert(last_card [1;4] 5 = -1);
    assert(last_card [1;4] 1 = 4); 
    assert(last_card [1;4;2;10;3;5;7;11] 1 = 5);
    assert(last_card [1;4;2;10;3;5;7;11] 3 = 10);
    assert(last_card [1;4;2;10;3;5;7;11] 7 = 11);
    print_endline "All run_last_card_test passed!" 
  
  let test_num_greater_than_sum () =
    assert ( num_greater_than_sum [] = 0);
    assert ( num_greater_than_sum [1;4;2;10]= 3);
    assert ( num_greater_than_sum [1;-1;2] = 2);
    assert ( num_greater_than_sum [1;4;2;10;3;-5;-7;11] = 4);
    assert ( num_greater_than_sum [5; -3; 3; 5; 3; -1; 0; 4; -3; 2; 5; -1; -2; 3; -3] = 2);

    
    print_endline "All test_num_greater_than_sum passed!" 
  
  let run_all_tests () = 
    test_leftover_count ();
    test_last_card_test ();
    test_num_greater_than_sum ();
    print_endline "All tests passed!" 

    let _ = run_all_tests ()  