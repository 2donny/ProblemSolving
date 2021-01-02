type exp = 
  | V of var
  | P of var * exp
  | C of exp * exp
and var = string

let variableStack var c_list = var::(c_list);;
(*VariableStack ["a"] "b" => ["a";"b"]*)


let rec checkEl c_list str = match c_list with
  |[] -> false
  |hd::tl -> if hd = str then true else checkEl tl str;;
  
let rec check : exp -> bool = fun exp -> 
  let rec checkHelper c_list exp = match exp with
    |V str -> if checkEl c_list str then true else false
    |P (str, exp') -> checkHelper (variableStack str c_list) exp'
    |C (exp1, exp2) -> if (checkHelper c_list exp1) && (checkHelper c_list exp2) then true 
                      else false
  in checkHelper [] exp;;
  
  

check (P ("a", P ("b", C (V "a", V "c"))));;
