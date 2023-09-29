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
     fun (sum, counter) h ->
  if sum > h then (sum + h, counter) else (sum + h, counter + 1) ) 
  (0, 0) lst in y

