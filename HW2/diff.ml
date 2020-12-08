type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec checkVariable lst x = match lst with
  |[] -> false
  |hd::tl -> match hd with
    |Var v -> if x = v then true else checkVariable tl x
    |_ -> checkVariable tl x;;


let rec diff : aexp * string -> aexp
= fun (exp, x) -> match exp with
  |Const n -> Const 0
  |Var var -> if var = x then Const 1 else Const 0
  |Power (mit, jisu) -> if mit = x then Times [Const jisu; Power (mit, (jisu-1))]
                      else Const 0
  |Times exp_list ->  if checkVariable exp_list x then begin match exp_list with (*checkVariable이 x를 하나라도 포함하면, True*)
    |[] -> Const 1
    |hd::tl -> Sum ([Times (diff (hd, x)::tl)] @ [Times (hd::[diff (Sum tl, x)])])
    end
(*      Sum (Times (diff (hd, x))::tl)::[Times hd::[Sum[diff (tl, x)]]]*)
    else Const 0
  |Sum exp_list -> match exp_list with
    |[] -> Const 0
    |hd::tl -> Sum (diff (hd, x)::[diff (Sum tl, x)]);;
      
diff (Sum [Power ("x", 2); Times [Var "x"]; Const 2], "x");;
