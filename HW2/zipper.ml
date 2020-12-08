let zipper : int list * int list -> int list
= fun (l1, l2) -> let result_list = [] in 
    let rec zipper_maker l1 l2 lst = match (l1, l2) with
      |([], []) -> lst
      |([], hd::tl) -> zipper_maker l1 tl (lst @ [hd])
      |(hd::tl, []) -> zipper_maker tl l2 (lst @ [hd])
      |(hd1::tl1, hd2::tl2) -> zipper_maker tl1 tl2 (lst @ [hd1;hd2]) in
        zipper_maker l1 l2 result_list;;
        
(*zipper ([1;3;5;7], [2;4]);;*)

