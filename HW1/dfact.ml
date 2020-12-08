let rec dfact : int -> int = fun n -> 
  if n >= 2 then n * (dfact (n-2))
  else 1;;
