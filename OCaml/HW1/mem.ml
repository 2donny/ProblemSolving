type btree =
	| Empty
	| Node of int * btree * btree;;

let rec mem : int -> btree -> bool
= fun n t -> match t with
  |Empty -> false
  |Node (v, t1, t2) -> match (v, t1, t2) with
    | (v, t1, t2) -> if v = n then true 
      else match (mem n t1, mem n t2) with 
        | (false, false) -> false
        | _ -> true;;
        
let t1 = Node (2, Empty, Empty);;
let t2 = Node(1, Node (4, Empty, Empty), Node (3, Empty, Empty));;

(*mem 4 t2;;*)

