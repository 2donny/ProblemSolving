let rec double : ('a -> 'a) -> 'a -> 'a = fun f -> 
  fun x -> f (f x);;

(**)
(*let inc x = x + 1;;*)
(*let mul x = x * 2;;*)
(**)
(**)
