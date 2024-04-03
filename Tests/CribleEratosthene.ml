(* Test pour la récursivité terminale et pour les appels de fonction *)
let rec generer = function
| 2 -> [2]
| n -> generer(n-1)@[n] ;;

(*
generer : int -> int list  
generer 6 ;;
- : int list = [2; 3; 4; 5; 6]
*)

let rec eliminer = function
| (a,x::q) -> if x mod a = 0 
				then eliminer (a,q)
				else x :: eliminer(a,q)
| (_ , []) -> [] ;;

(*
eliminer : int * int list -> int list
eliminer (2 , [1;2;3;4;5;6] );;
- : int list = [1; 3; 5] 
*)

let rec eratosAux = function
| (a :: q) -> a :: eratosAux(eliminer(a,q)) 
| [] -> [] ;;

let eratos = fun n ->
	let liste = generer(n) in 
	eratosAux liste ;;
(*eratos : int -> int list
eratos 10 ;;
- : int list = [2; 3; 5; 7] *)

eratos 20 ;;
(*- : int list = [2; 3; 5; 7; 11; 13; 17; 19]) *)

(*
let rec jumeaux = function
| (a::b::q) -> if b-a = 2 then (a,b):: jumeaux(b::q) 
				else jumeaux (b::q)
| _ -> [] ;;
(*jumeaux : int list -> (int * int) list*)

jumeaux (eratos 20) ;;
(*- : (int * int) list = [3, 5; 5, 7; 11, 13; 17, 19]*)

let listeJumeaux = fun n -> jumeaux (eratos n) ;;
(*listeJumeaux : int -> (int * int) list *)

listeJumeaux 20 ;;
(*- : (int * int) list = [3, 5; 5, 7; 11, 13; 17, 19] *)
*)


