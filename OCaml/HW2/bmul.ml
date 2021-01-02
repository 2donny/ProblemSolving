type digit = ZERO | ONE
type bin = digit list

exception Unexpected_num;;

let numToDigit n = match n with
  |0 -> ZERO
  |1 -> ONE
  |_ -> raise Unexpected_num;;

let rec cnt acc lst = match lst with
  |[] -> acc
  |hd::tl -> cnt (acc+1) tl;;
  
(*cnt 0 [ONE] => 1*)
let int_exp x y = (float_of_int x) ** (float_of_int y) |> int_of_float;;

let rec digitToNum lst = match lst with
  |[] -> 0
  |hd::tl -> let count = cnt 0 lst in
      if hd = ZERO then digitToNum tl
      else let new_cnt = count - 1 in 
        int_exp 2 new_cnt + digitToNum tl;;

let digitalize n = 
  let a = n in
  	 let rec alu a list = 
    	 match a with
    	 |0 -> if list = [] then [ZERO] else list
    	 |_ -> alu (a/2) (numToDigit(a mod 2)::list) in
    	    alu a [];;

let bmul : bin -> bin -> bin = fun a b -> 
  let n1 = digitToNum a in
  let n2 = digitToNum b in
  digitalize (n1*n2);;
              
(*bmul [ONE;ZERO;ONE;ONE] [ONE;ZERO];;*)

