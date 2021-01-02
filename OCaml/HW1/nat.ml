type nat = ZERO | SUCC of nat;;

let rec countSUCC nat = match nat with
  |ZERO -> 0
  |SUCC (nat') -> 1 + countSUCC nat';;

let natadd : nat -> nat -> nat
= fun n1 n2 -> 
  let num1 = countSUCC n1 in
  let num2 = countSUCC n2 in
    let rec result num = match num with
      |0 -> ZERO
      |_ -> SUCC (result (num-1)) in
        result (num1 + num2);;
        
let natmul : nat -> nat -> nat
= fun n1 n2 -> 
  let num1 = countSUCC n1 in
  let num2 = countSUCC n2 in
    let rec result num = match num with
      |0 -> ZERO
      |_ -> SUCC (result (num-1)) in
        result (num1 * num2);;
      
(*let two = SUCC (SUCC ZERO);;*)
(*let three = SUCC (SUCC (SUCC ZERO));;*)
(*let four = SUCC (SUCC (SUCC (SUCC ZERO)));;*)
(**)
(*natadd two three;;*)
(*natmul two four;;*)
(**)


