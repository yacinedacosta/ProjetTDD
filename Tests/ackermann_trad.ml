(* Traditional version of Ackermann function *)

let rec ack (m : int) (n : int) : int = 
  if m = 0
  then n + 1
  else if n = 0
  then ack (m - 1) 1
  else ack (m - 1) (ack m (n - 1))
;;

ack 3 2;;
