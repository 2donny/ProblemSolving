let binarize n = 
  let a = n in
  	 let rec alu a list = 
    	 match a with
    	 |0 -> list
    	 |_ -> alu (a/2) (a mod 2::list) in
    	    alu a [];;