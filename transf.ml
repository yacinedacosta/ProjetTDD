  (* Transformation of Caml code to Python code *)

open Lang


module StringSet = Set.Make(String)

(*Pour savoir si une fonction est appelée dans une expression*)
(*let containsF f exp = match exp with*)
(*
let is_tailrec_expr f exp = match exp with
  |IfThenElse (condition, exp1, exp2 )-> not (containsF f condition) &&
                        is_tailrec_expr f exp1 && is_tailrec_expr f exp2
  |BinOp (op, exp1, exp2) -> is_tailrec_expr f exp1 && is_tailrec_expr f exp1
  |CallE [] -> true    
  |CallE (expr::liste) -> is_tailrec_expr f expr && is_tailrec_expr f liste
  |_ -> true
*)

(* TODO: implement *)
let rec names_expr = StringSet.empty

(* TODO: implement *)
let transf_prog (Prog(fdfs, e)) = Prog(fdfs, e)

