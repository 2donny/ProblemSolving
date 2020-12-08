
(*  check함수가 True를 리턴하면 el가 result_list에 없다는 뜻*)
(* 중복된 것이 있으면 False를 리턴*)
let rec check el lst = match lst with
  |[] -> true
  |hd::tl -> if el <> hd then check el tl else false


let rec checkEL tail result_list = match tail with
  |[] -> result_list
  |hd::tl -> if (check hd result_list) then checkEL tl (result_list @ [hd])
             else checkEL tl result_list;;
  
  
let uniq : 'a list -> 'a list = fun lst -> match lst with
  |[] -> lst
  |hd::tl -> checkEL tl [hd];;
  

(*uniq [5;6;4;2;5;61];;*)
   
