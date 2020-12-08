type program = exp
and exp = 
  | UNIT
  | TRUE
  | FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | NIL
  | CONS of exp * exp      
  | APPEND of exp * exp
  | HEAD of exp
  | TAIL of exp
  | ISNIL of exp
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | LETMREC of (var * var * exp) * (var * var * exp) * exp
  | PROC of var * exp
  | CALL of exp * exp
  | PRINT of exp
  | SEQ of exp * exp
and var = string

type value = 
  | Unit 
  | Int of int 
  | Bool of bool 
  | List of value list
  | Procedure of var * exp * env 
  | RecProcedure of var * var * exp * env
  | MRecProcedure of var * var * exp * var * var * exp * exp
and env = (var * value) list

let rec fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
= fun f accu lst ->
  match lst with
  | [] -> accu
  | hd::tl -> fold_left f (f accu hd) tl

let rec map : ('a -> 'b) -> 'a list -> 'b list
= fun f lst ->
  match lst with
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)

let rec string_of_value v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | List lst -> "[" ^ fold_left (fun s x -> s ^ ", " ^ x) "" (map string_of_value lst) ^ "]"
  | _ -> "(functional value)"

exception UndefinedSemantics

let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec lookup_env x e = 
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " is not bound in env")) 
  | (y,v)::tl -> if x = y then v else lookup_env x tl

let rec eval : exp -> env -> value
=fun exp env ->
  match exp with
  | PRINT e -> (print_endline (string_of_value (eval e env)); Unit)
  | UNIT -> Unit
  | TRUE -> Bool true
  | FALSE -> Bool false
  | CONST n -> Int n
  | VAR x -> lookup_env x env
  | ADD (e1, e2) -> 
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
        | Int n1, Int n2 -> Int (n1 + n2)
        | _ -> raise UndefinedSemantics)
  | SUB (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
        | Int n1, Int n2 -> Int (n1 - n2)
        | _ -> raise UndefinedSemantics)
  | MUL (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
        | Int n1, Int n2 -> Int (n1 * n2)
        | _ -> raise UndefinedSemantics)
  | DIV (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
        | Int n1, Int n2 -> Int (n1 / n2)
        | _ -> raise UndefinedSemantics)
  | EQUAL (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
          | Int n1, Int n2 -> if n1 = n2 then Bool true else Bool false
          | _ -> raise UndefinedSemantics)
  | LESS (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in 
      (match v1, v2 with
          | Int n1, Int n2 -> if n1 < n2 then Bool true else Bool false
          | _ -> raise UndefinedSemantics)
  | NOT e1 ->
    let v1 = eval e1 env in (match v1 with
      | Bool b -> Bool (not b)
      | _ -> raise UndefinedSemantics)
  | NIL -> List []
  | APPEND (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in (match v1, v2 with
      | List l1, List l2 -> List (l1@l2))
  | HEAD e1 -> 
    let v1 = eval e1 env in (match v1 with
      |List lst -> (match lst with 
        |hd::tl -> hd))
  | TAIL e1 ->
    let v1 = eval e1 env in (match v1 with
      |List lst -> (match lst with 
        |hd::tl -> List tl))
  | ISNIL e1 ->
    let v1 = eval e1 env in (match v1 with
      |List lst -> (match lst with 
        |[] -> Bool true
        |hd::tl -> Bool false))        
  | IF (e1, e2, e3) -> 
    begin
      match (eval e1 env) with
        | Bool true -> (eval e2 env)
        | Bool false -> (eval e3 env)
      end
  | CALL (e1, e2) ->
    let v2 = eval e2 env in
    let v1 = eval e1 env in (match v1 with
      |Procedure (x, e, p') -> let new_env = (extend_env (x, v2) p') in
        eval e new_env
      |RecProcedure (f, x, e, p') -> 
        let new_env = (extend_env (x, v2) p') in
        let new_env' = extend_env (f, v1) new_env in
          eval e new_env'
(*      |MRecProcedure (f, x, e3, g, y, e4, p') -> *)
(*        let new_env = extend_env (x, v2) p' in*)
(*        let new_env' = (extend_env (f, MRecProcedure (f, x, e3, g, y, e4, p')) new_env) in*)
(*        let newer_env = (extend_env (g, MRecProcedure (g, y, eg, f, x, ef, p')) new_env') in*)
(*            eval e3 newer_env*)
        )
  | PROC (x, e1) ->  
    Procedure (x, e1, env)
    
  
  | LETREC (v1, v2, e1, e2) ->
    let closure = RecProcedure (v1, v2, e1, env) in
    let new_env = extend_env (v1, closure) env in
      eval e2 new_env
  | LET (x, e2, e3) -> 
    let v2 = eval e2 env in
      let new_env = (extend_env (x, v2) env) in 
        eval e3 new_env
        
(*  | LETMREC ((f1, x1, e1), (f2, x2, e2), ee) ->*)
(*    let new_env = extend_env (f1, MRecProcedure (f1, x1, e1, f2, x2, e2, env)) env in*)
(*    eval ee (extend_env (f2, MRecProcedure (f2, x2, e2, f1, x1, e1, env)) new_env)*)
(*    *)
    
  | LETREC (f, x, e1, e2) ->
    let new_env = extend_env (f, RecProcedure (f, x, e1, env)) env in
    eval e2 new_env
    
  | SEQ (e1, e2) ->
    eval e2 env 
  | CONS (e1, e2) ->
    let v1 = eval e1 env in 
    let v2 = eval e2 env in match v2 with
      | List [] -> List [v1]
      | List l -> List (v1::l)
    ;;
let runml : program -> value
=fun pgm -> eval pgm empty_env;;

runml (LET ("fix",
PROC ("f",
CALL
(PROC ("x",
CALL (VAR "f", PROC ("y", CALL (CALL (VAR "x", VAR "x"), VAR "y")))),
PROC ("x",
CALL (VAR "f", PROC ("y", CALL (CALL (VAR "x", VAR "x"), VAR "y")))))),
LET ("f",
CALL (VAR "fix",
PROC ("f",
PROC ("x",
IF (EQUAL (VAR "x", CONST 0), CONST 1,
MUL (CALL (VAR "f", SUB (VAR "x", CONST 1)), VAR "x"))))),
CALL (VAR "f", CONST 10))));;
