let rec checkEL head lst = match lst with
  |[] -> true
  |hd::tl -> if (head <> hd) then checkEL head tl 
             else false;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> match (l1, l2) with
  |([], l2) -> l2
  |(hd::tl, l2) -> if (checkEL hd l2) then app tl (l2 @ [hd])
        else app tl l2;;
          

(*app [4;5;2;1] [1;2;3;4];;*)