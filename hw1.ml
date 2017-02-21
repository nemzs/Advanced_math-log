type peano = Z | S of peano;; (* типы необходимо копировать в реализацию *)
type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;

let rec peano_of_int x = match x with
	| 0 -> Z
	| x -> S(peano_of_int (x - 1));;

let rec int_of_peano p = match p with
    Z -> 0
  | S x -> 1 + int_of_peano x;;

let inc x = match x with
	| x -> S x;;
	
let rec add x y = match x, y with
	| (x, Z) -> x
	| (Z, y) -> y
	| (S x, y) -> S( add x y);;
	
let rec mul x y = match x, y with
	| (x , Z) -> Z
	| (Z , y) -> Z
	| (x , S dy) -> add(mul x dy) x;;
	
let rec sub x y = match x, y with
	| (x, Z) -> x
	| (Z, y) -> Z
	| (S a, S b) -> sub a b;;
	
let rec power x y = match x, y with
	| (x, Z) -> S Z
	| (Z, y) -> Z
	| (x, S y) -> mul x (power x y);;
                     
let rev x = failwith "Not implemented";;
let merge_sort x = failwith "Not implemented";;
                     
let string_of_lambda x = failwith "Not implemented";;
let lambda_of_string x = failwith "Not implemented";;