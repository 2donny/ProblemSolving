let prime : int -> bool
= fun n -> if n < 0 then false else match n with  (* n이 음수이면 false 리턴 *)
    |0 |1 -> false
    |2 -> true
    |3 -> true
    |_ -> let check = n-1 in  (* 3보다 큰 n에 대하여 isPrimeOrNot 함수를 정의*)
          let rec isPrimeOrNot n check =
            match (n, check) with
            |(n, _) -> 
              if n mod 2 = 0 then false 
              else if n mod 3 = 0 then false (*n이 2, 3이 아닌 2, 3의 배수이면 소수가 아니므로 더 진행할 필요없이 false를 리턴*)
              else match check with
                  |1 -> true
                  |_ -> match n mod check with
                        |0 -> false
                        |_ -> isPrimeOrNot n (check-1) in isPrimeOrNot n check
                        ;;