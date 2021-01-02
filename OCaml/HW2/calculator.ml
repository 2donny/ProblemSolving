 type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

exception Invalid_exp
  let rec calculator : exp -> int = fun exp -> 
      let rec calHelper exp n =
        match exp with
        |X -> n
        |INT n' -> n'
        |ADD(e1, e2) -> (calHelper e1 n) + (calHelper e2 n)
        |SUB(e1, e2) -> (calHelper e1 n) - (calHelper e2 n)
        |MUL(e1, e2) -> (calHelper e1 n) * (calHelper e2 n)
        |DIV(e1, e2) -> (calHelper e1 n) / (calHelper e2 n)
        |_ -> raise Invalid_exp in match exp with
          | X -> raise Invalid_exp
          | INT n' -> n'
          | ADD(e1,e2) -> (calculator e1) + (calculator e2)
          | SUB(e1,e2) -> (calculator e1) - (calculator e2)
          | MUL(e1,e2) -> (calculator e1) * (calculator e2)
          | DIV(e1,e2) -> (calculator e1) / (calculator e2)
          | SIGMA(e1,e2,e3) -> let start = (calculator e1) in
                              let ends = (calculator e2) in
                            if (start > ends) then 0
                            else (calHelper e3 start) + calculator (SIGMA(INT (start + 1), e2, e3));;
      
(*calculator (SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1)));;*)