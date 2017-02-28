open Hw2;;
(* 
	author nemzs
	testing field
	*)
	

let rec print_list_int x = match x with
  | [] -> print_string ""
  | h::sx -> print_int h; print_string " "; print_list_int sx;;
  
print_list_int [1;7;2;3;7;8;2];;
print_string " : ";;
print_list_int (rev [1;7;2;3;7;8;2]);;
print_string " : ";;
print_list_int (merge_sort [1;7;1;2;7;8;3]);;
print_string " : ";;
print_list_int (merge_sort (rev [1;7;1;2;7;8;3]));;
  (*
  print_string (Hw1.string_of_lambda (Hw1.lambda_of_string "\\x.\\y.x"));;
*)