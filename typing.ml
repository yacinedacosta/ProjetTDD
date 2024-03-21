open Lang
  
(* Environments *)
type environment = 
    {localvar: (vname * tp) list; 
     funbind: (vname * fpdecl) list}

let tp_fun fpdecl = match fpdecl with
| FPdecl(tp, _, _) -> tp 

(* tp_var_local : (vname * tp) list -> vname -> tp option      avec tp option = Some tp ou None*)
let rec tp_var_local envLocal var = match envLocal with
| (vname, tp) :: reste -> 
        if vname = var
        then Some tp
        else tp_var_local reste var
| [] -> None 

(* tp_var_Funbind : (vname * fpdecl) list -> vname -> tp option *)
let rec tp_var_Funbind envFunbind var = match envFunbind with
| (vname, fpdecl) :: reste ->
        if vname = var
        then Some (tp_fun fpdecl)
        else tp_var_Funbind reste var
| [] -> None  

(* tp_var : env -> vname -> tp *)
let rec tp_var env var = match tp_var_local env.localvar var with
|None -> (match tp_var_Funbind env.funbind var with
        |None -> failwith "La variable n'est pas dans l'environnement"
        |Some t -> t)
|Some t -> t

(*TO DO*)
let tp_application fct liste_args = IntT

(* tp_binop : operateur -> tp -> tp -> tp *)
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
        
(* tp_expr : env * exp -> tp *)
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
|_-> failwith "Mais qu'est-ce que c'est ???"

(* TODO: implement *)
let tp_prog (Prog (fdfs, e)) = IntT