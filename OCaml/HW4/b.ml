type exp =
  | NUM of int | TRUE | FALSE | UNIT
  | VAR of id
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | SEQ of exp * exp                 (* sequence *)
  | IF of exp * exp * exp            (* if-then-else *)
  | WHILE of exp * exp               (* while loop *)
  | LETV of id * exp * exp           (* variable binding *)
  | LETF of id * id list * exp * exp (* procedure binding *)
  | CALLV of id * exp list           (* call by value *)
  | CALLR of id * id list            (* call by referenece *)
  | RECORD of (id * exp) list        (* record construction *)
  | FIELD of exp * id                (* access record field *)
  | ASSIGN of id * exp               (* assgin to variable *)
  | ASSIGNF of exp * id * exp        (* assign to record field *)
  | WRITE of exp
and id = string


type loc = int
type value =
| Num of int
| Bool of bool 
| Unit
| Record of record 
and record = (id * loc) list
type memory = (loc * value) list
type env = binding list
and binding = LocBind of id * loc | ProcBind of id * proc
and proc = id list * exp * env

(*let env = [ProcBind (f, ([x1; x2; x3], e1, env))];;*)
(*let env = [(LocBind ('x', 3)); (LocBind ('y', 4))];;*)




(*let (r1, mem1) = eval env mem e1 in*)
(*let (v2, mem2) = eval env mem1 e2 in*)
(*let record_value = lookup_record id record in*)

(********************************)
(*     Handling environment     *)
(********************************)

let rec lookup_loc_env : id -> env -> loc
= fun x env ->
  match env with
  | [] -> raise(Failure ("Variable "^x^" is not included in environment"))
  | hd::tl ->
    begin match hd with
    | LocBind (id,l) -> if(x=id) then l else lookup_loc_env x tl
    | ProcBind _ -> lookup_loc_env x tl
    end

let rec lookup_proc_env : id -> env -> proc
= fun x env ->
  match env with
  | [] -> raise(Failure ("Variable "^x^" is not included in environment"))
  | hd::tl ->
    begin match hd with
    | LocBind _ -> lookup_proc_env x tl
    | ProcBind (id,binding) -> if (x=id) then binding else lookup_proc_env x tl
    end
    
(*// proc : ([x1; x2; x3], exp, env)*)



(*[y1; y2; y3] [x1; x2; x3]*)




let extend_env : binding -> env -> env
= fun e env -> e::env

let extender_env: env -> env -> env
= fun env1 env2 -> env1 @ env2

let empty_env = []

(***************************)
(*     Handling memory     *)
(***************************)

let rec lookup_mem : loc -> memory -> value
= fun l mem ->
  match mem with
  | [] -> raise(Failure ("location "^(string_of_int l)^" is not included in memory"))
  | (loc,v)::tl -> if(l=loc) then v else lookup_mem l tl

let extend_mem : (loc * value) -> memory -> memory
= fun (l,v) mem -> (l,v)::mem



let extender_mem: memory -> memory -> memory 
= fun m1 m2 -> m1 @ m2

let empty_mem = []




(***************************)
(*     Handling record     *)
(***************************)

let rec lookup_record : id -> record -> loc
= fun id record -> 
  match record with
    | [] -> raise(Failure ("field "^ id ^" is not included in record"))
    | (x,l)::tl -> if(id=x) then l else lookup_record id tl


let extend_record : (id * loc) -> record -> record
= fun (x,l) record -> (x,l)::record

let empty_record = []

(***************************)

let counter = ref 0
let new_location () = counter:=!counter+1;!counter

let mem_counter = ref 0
let new_mem_cnt () = mem_counter:=!mem_counter+1;!mem_counter

exception NotImplemented
exception UndefinedSemantics

let rec list_fold2 : ('a -> 'b -> 'c -> 'c)-> 'a list -> 'b list -> 'c -> 'c
= fun func l1 l2 acc ->
  match (l1,l2) with
  | ([],[]) -> acc
  | (hd1::tl1,hd2::tl2) -> list_fold2 func tl1 tl2 (func hd1 hd2 acc)
  | _ -> raise (Failure "two lists have different length")


let rec list_fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
= fun func l acc ->
  match l with
  | [] -> acc
  | hd::tl -> list_fold func tl (func hd acc)


let value2str : value -> string
= fun v ->
  match v with
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unit -> "unit"
  | Record _ -> "record" 

let rec eval_aop : env -> memory -> exp -> exp -> (int -> int -> int) -> (value * memory)
= fun env mem e1 e2 op ->
  let (v1,mem1) = eval env mem e1 in
  let (v2,mem2) = eval env mem1 e2 in
  match (v1,v2) with
  | (Num n1, Num n2) -> (Num (op n1 n2), mem2)
  | _ -> raise (Failure "arithmetic operation type error")

and eval : env -> memory -> exp -> (value * memory) 
=fun env mem e -> 
  match e with
  | WRITE e -> 
    let (v1,mem1) = eval env mem e in
    let _ = print_endline(value2str v1) in
    (v1,mem1)
  | TRUE -> (Bool true, mem)
  | FALSE -> (Bool false, mem)
  | UNIT -> (Unit, mem)
  | NUM n -> (Num n, mem)
  | VAR id -> let loc = lookup_loc_env id env in
             let value = lookup_mem loc mem in
             (value, mem)
  | ADD (e1, e2) -> eval_aop env mem e1 e2 (fun x y -> x + y)
  | SUB (e1, e2) -> eval_aop env mem e1 e2 (fun x y -> x - y)
  | MUL (e1, e2) -> eval_aop env mem e1 e2 (fun x y -> x * y)
  | DIV (e1, e2) -> eval_aop env mem e1 e2 (fun x y -> x / y)
  | EQUAL (e1, e2) -> 
    let (v1, mem1) = eval env mem e1 in
    let (v2, mem2) = eval env mem1 e2 in
    begin
      match (v1, v2) with
        |(Num n1, Num n2) -> if(n1 = n2) then (Bool true, mem2) else (Bool false, mem2)
        |(Bool b1, Bool b2) -> if(b1 = b2) then (Bool true, mem2) else (Bool false, mem2)
        |(Unit, Unit) -> (Bool true, mem2)
        |_ -> (Bool false, mem2)
    end
  | LESS (e1, e2) -> 
    let (v1, mem1) = eval env mem e1 in
    let (v2, mem2) = eval env mem1 e2 in
    begin
      match (v1, v2) with
        |(Num n1, Num n2) -> if(n1 < n2) then (Bool true, mem2) else (Bool false, mem2)
        |_ -> raise UndefinedSemantics
    end
  | NOT e -> begin match (eval env mem e) with
    | (Bool true, mem1) -> (Bool false, mem1)
    | (Bool false, mem1) -> (Bool true, mem1)
    | _ -> raise UndefinedSemantics
    end
    
  | ASSIGN (id, e1) ->
    let (v1, mem1) = eval env mem e1 in
    let loc = lookup_loc_env id env in
    let new_mem = extend_mem (loc, v1) mem1 in
    (v1, new_mem)
  | SEQ (e1, e2) -> 
    let (v1, mem1) = eval env mem e1 in
    let (v2, mem2) = eval env mem1 e2 in
    (v2, mem2)
    
  | IF (e1, e2, e3) -> begin match (eval env mem e1) with
    | (Bool true, mem1) -> let (v, mem2) = eval env mem1 e2 in (v, mem2)
    | (Bool false, mem1) -> let (v, mem2) = eval env mem1 e3 in (v, mem2)
    | _ -> raise UndefinedSemantics
    end
  | WHILE (e1, e2) -> begin match (eval env mem e1) with
    | (Bool false, mem1) -> (Unit, mem1)
    | (Bool true, mem1) -> let (v1, m) = eval env mem1 e2 in
                          let (v2, m2) = eval env m (WHILE (e1, e2)) in
                            (v2, m2)
    | _ -> raise UndefinedSemantics
    end
  | LETV (id, e1, e2) ->
    let (v, m1) = eval env mem e1 in
    let new_loc = new_location () in
    let ex_env = extend_env (LocBind (id, new_loc)) env in
    let ex_mem = extend_mem (new_loc, v) m1 in
    let (v2, m2) = eval ex_env ex_mem e2 in
    (v2, m2)
  | LETF (id, idLst, e1, e2) ->
    let extended_env = extend_env (ProcBind (id, (idLst, e1, env))) env in
    let (v, mem1) = eval extended_env mem e2 in
      (v, mem1)
  | FIELD (e, id) -> begin match (eval env mem e) with
    | (Record r1, m1) -> let new_loc = lookup_record id r1 in
                        let new_val = lookup_mem new_loc m1 in
                        (new_val, m1)
    | _ -> raise UndefinedSemantics
    end
  | ASSIGNF (e1, id, e2) -> begin match (eval env mem e1) with
    | (Record r1, m1) -> begin match (eval env m1 e2) with
      | (v2, m2) -> let new_loc = lookup_record id r1 in
                    let extended_mem = extend_mem (new_loc, v2) m2 in
                    (v2, extended_mem)
      end
    | (_, _) -> raise UndefinedSemantics
    end
  | RECORD lst -> begin match lst with
    | [] -> (Unit, mem)
    | _ -> 
      begin
      let rec helper = fun expLst memory1 -> match expLst with
      |[] -> ([], [])
      |hd::tl -> match hd with
        | (id, ex) ->
          let (v1, m1) = eval env memory1 ex in
          let new_loc = new_location () in
          let new_record = extend_record (id, new_loc) empty_record in
          let new_memory = extend_mem (new_loc, v1) m1 in
          
          let (next_record, next_memory) = helper tl m1 in
          (new_record @ next_record, new_memory @ next_memory) in
          
          let (new_record, new_memo) = helper lst mem in
            (Record new_record, new_memo)
            end
      end
        
  | CALLV (id, expLst) ->
    begin match expLst with
      |hd::tl ->
        let (parameterLst, exp1, env1) = lookup_proc_env id env in
        let (new_env, new_mem) = list_fold2 
            (fun e x (new_e, new_m) -> 
              let (v1, m1) = eval env mem e in
              let new_loc = new_location () in
              let new_env = extend_env (LocBind (x, new_loc)) env1 in
              let new_mem = extend_mem (new_loc, v1) new_m in
              (new_e @ new_env, (new_m @ new_mem))
            )
            expLst parameterLst (env1, mem) in
            let (v3, m3) = eval new_env new_mem exp1 in
            (v3, m3)
      | _ -> raise UndefinedSemantics
      end
  | CALLR (id, idLst) ->
    let (parameterLst, exp1, env1) = lookup_proc_env id env in
    let newer_env = list_fold2 
            (fun y x acc -> 
              let new_loc = lookup_loc_env y env in
              let new_env = extend_env (LocBind (x, new_loc)) env1 in
              acc @ (new_env)
            )
            idLst parameterLst [] in
            let (value, new_mem) = eval newer_env mem exp1 in
            (value, new_mem)
  
  
  | _ -> raise NotImplemented 

let runb : exp -> value 
=fun exp -> let (v, _) = eval empty_env empty_mem exp in v;;

runb (RECORD ([("x", NUM 10); ("z", NUM 13); ("z", NUM 22)]));;

