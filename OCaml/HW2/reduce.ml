exception Invalid_lists;;

let rec reduce : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
= fun f l1 l2 accu -> match (l1, l2) with
  |([], []) -> accu
  |(hd1::tl1, hd2::tl2) -> let new_accu = f hd1 hd2 accu in 
        reduce f tl1 tl2 new_accu
  |(_, _) -> raise Invalid_lists;;
  
(*reduce (fun x y z -> x * y + z) [1;2;3] [0;1;2] 1;;  *)
(*reduce (fun x y z -> x * y + z) [1] [] 0;;*)
  
  
        
        
