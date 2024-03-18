
open Lang
  
(* Environments *)

type environment = 
    {localvar: (vname * tp) list; 
     funbind: (vname * fpdecl) list}

let tp_fun fpdecl = match fpdecl with
| FPdecl(tp, _, _) -> tp 

let rec tp_var_local envLocal var = match envLocal with
| (vname, tp) :: reste -> 
        if vname = var
        then Some tp
        else tp_var_local reste var
| [] -> None 

let rec tp_var_Funbind envFunbind var = match envFunbind with
| (vname, fpdecl) :: reste ->
        if vname = var
        then Some (tp_fun fpdecl)
        else tp_var_Funbind reste var
| [] -> None  

let rec tp_var env var = match tp_var_local env.localvar var with
|None -> (match tp_var_Funbind env.funbind var with
        |None -> failwith "La variable n'est pas dans l'environnement"
        |Some t -> t)
|Some t -> t

(*|((nom, tp)::reste) -> if nom=var then tp else tp_var {localvar = reste; funbind = env.funbind} var
|[]-> failwith "La variable n'est pas dans l'environnement !"
*)(*liste assoc*)

(*TO DO*)
let tp_application fct liste_args = BoolT

let tp_binop operateur toperande1 toperande2 = match operateur with
| BArith(_) -> 
        if toperande1=IntT && toperande2=IntT 
        then IntT 
        else failwith "erreur"
| BCompar(_) -> 
        if toperande1 = toperande2
        then toperande1
        else failwith "erreur"
| BLogic(_) -> 
        if toperande1=BoolT && toperande2=BoolT
        then BoolT
        else failwith "erreur"
        

let rec tp_expr env exp = match exp with
(* Constantes *)
|Const (BoolV _) -> BoolT  
|Const (IntV _) -> IntT
(* Variables *)
|VarE (e) -> tp_var env e
(* Fonctions *)
|CallE (e1::liste_exp) -> 
        let fct = tp_expr env e1 in 
        let arguments = List.map (tp_expr env) liste_exp in
        tp_application fct arguments
(* pour que le filtrage soit exhaustif *)
|CallE(_) -> failwith "Liste d'arguments vide !"
(* Opérations binaires *)
|BinOp (operateur, operande1, operande2) -> 
        tp_binop  operateur (tp_expr env operande1) (tp_expr env operande2)
(* Expression conditionnelle *)
|IfThenElse (e1, e2, e3) -> 
        if tp_expr env e1 = BoolT && tp_expr env e2 = tp_expr env e3 
        then tp_expr env e2 
        else failwith "Types différents dans les deux branches !"
|_-> failwith "Mais qu'est-ce que c'est que ça ???"

(* TODO: implement *)
let tp_prog (Prog (fdfs, e)) = IntT
