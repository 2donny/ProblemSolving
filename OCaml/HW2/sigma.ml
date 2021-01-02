let sigma : (int -> int) -> int -> int -> int = fun f a b -> 
  let rec solve f a b sum = if a > b then sum 
   else let new_sum = sum + (f a) in solve f (a+1) b new_sum in
    solve f a b 0;;
   
   
(*sigma (fun x -> x)  10;;*)
(*sigma (fun x -> x * x )  7;;*)
    
    
    