type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Equal of exp * exp
  | Imply of formula * formula
  and exp =
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp ;;  

let rec evalExp: exp -> int = fun exp -> match exp with
  | Num (n) -> n
  | Plus (ex1, ex2) -> evalExp ex1 + evalExp ex2
  | Minus (ex1, ex2) -> evalExp ex1 - evalExp ex2
    
let rec eval: formula -> bool = fun fml -> match fml with
  | True -> true
  | False -> false
  | Not ( f ) -> not (eval f)
  | AndAlso (f1, f2) -> eval f1 && eval f2
  | OrElse (f1, f2) -> eval f1 || eval f2
  | Equal (ex1, ex2) -> evalExp ex1 = evalExp ex2
  | Imply (f1, f2) -> match (eval f1, eval f2) with
    | (true, false) -> false
    | _ -> true;;

(**)
(*eval (Imply (Imply (True, False), True));;*)
(*eval (Equal (Num 1, Plus (Num 2, Num 1)));;*)
(*eval (Equal (Num 1, Minus (Num 2, Num 1)));;*)
