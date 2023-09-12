let multiply (x: int) (y: int) : string = 
  (string_of_int x) ^ " multiplied by " ^ (string_of_int y) ^ " is " ^ (string_of_int (x * y))

let multiply2  (x: int) (y: int) : string = 
(string_of_int y) ^ " multiplied by " ^ (string_of_int x) ^ " is " ^ (string_of_int (x * y))

let rec multHelper (i: int) (n: int) : string list = 
  if i > n then []
  else multiply2 i n  :: multHelper (i + 1) n

let multStringTable (n: int) :string list   =
    multHelper 1 n
(*
cd into current file directory  ->   cd /Users/cindy/Desktop
run utop -> utop
use utop to run file -> #use "fileName";;
execute function  -> finctionName parameter... ;;
*)






   
    
