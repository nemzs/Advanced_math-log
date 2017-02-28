type peano = Z | S of peano;; 
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

let rec rr x y = match x, y with
  |([], y) -> y
  |(h::sx, y) -> (rr sx (h::y));;

let rev x = (rr x []);;
let rec split l r i = match i with
  | 0 -> (rev l, r)
  | i -> match r with 
    | [] -> failwith "Array lower than range"
    | (h::sr) -> split (h::l) sr (i-1);;

let first x = match x with
  | (a, b) -> a;;
let second x = match x with
  | (a, b) -> b;;

let rec merge l r z = match l, r with
  | ([], []) -> rev z 
  | (hl::sl, []) -> merge sl [] (hl::z)
  | ([], hr::sr) -> merge [] sr (hr::z)
  | (hl::sl, hr::sr) -> if hl < hr then merge sl r (hl::z) else merge l sr (hr::z);;

let rec merge_sort x = 
  let sz = List.length x in
  let sp = split [] x (sz / 2) in
    match x with
      | _ -> if sz < 2 then x else 
        (merge 
           (merge_sort (first sp))
           (merge_sort (second sp)) []);;

let string_of_lambda x = failwith "Not implemented";;
let lambda_of_string x = failwith "Not implemented";;