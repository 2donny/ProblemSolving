let rec drop: ('a -> bool) -> 'a list -> 'a list = 
  fun f lst -> match lst with
    |[] -> []
    |hd::tl -> match f hd with
      |true -> drop f tl
      |false -> lst;;
      

(*drop (fun x -> x mod 2 = 1) [2];;*)
(*drop (fun x -> x > 5) [1;3;7];;*)


