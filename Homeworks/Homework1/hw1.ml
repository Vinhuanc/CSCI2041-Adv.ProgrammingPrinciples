let rec generate_duck_helper (x:int) (n:int):string list =
  match x with
  | 0 -> ["Mama duck went swimming one day"; "Over the hills and far away";"The mama duck said, \"Quack, quack, quack, quack\"";"And all " ^ string_of_int n ^ " little ducks came back"]
  | 1 -> ["1 little duck went swimming one day";"Over the hills and far away";"The mama duck said, \"Quack, quack, quack, quack\"";"And then no more little ducks came back"] @ generate_duck_helper (x - 1) n
  | 2 -> (string_of_int x ^ " little ducks went swimming one day") ::["Over the hills and far away";"The mama duck said, \"Quack, quack, quack, quack\"";"And only " ^ string_of_int (x - 1) ^ " little duck came back"] @ generate_duck_helper (x - 1) n
  | _ -> (string_of_int x ^ " little ducks went swimming one day") ::["Over the hills and far away";"The mama duck said, \"Quack, quack, quack, quack\"";"And only " ^ string_of_int (x - 1) ^ " little ducks came back"] @ generate_duck_helper (x - 1) n

let generate_duck_verse (n:int):string=
  String.concat "\n" (generate_duck_helper n n)

let print_duck_verse (n:int):unit =
  print_endline (generate_duck_verse n)