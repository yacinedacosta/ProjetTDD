let Fibonacci (n : int) =
	let rec fiboAux = fun 
	| (_,_,0) -> []
	| (a,b,n) -> a::fiboAux(b,a+b,n-1)
	in fiboAux (1,1,n) ;; 
(*Fibonacci : int -> int list = <fun>*)

(*Fibonacci 8 ;;*)
(*- : int list = [1; 1; 2; 3; 5; 8; 13; 21]*)