  (* Transformation of Caml code to Python code *)

open Lang


module StringSet = Set.Make(String)

(*Fonction vue en cours*)
(* Renvoie dans un ensmble les noms de variables utilisés dans l'expression *)
let rec names_expr = function
    Const(_) -> StringSet.empty
    (* Pas de variables dans une constante -> vide*)
   | VarE v -> StringSet.singleton v
   (* Une seule variable -> singleton*)
   | BinOp(_, e1, e2) -> StringSet.union (names_expr e1) (names_expr e2)
   (* Opération binaire -> on extrait les noms de variables des sous-expressions *)
   | IfThenElse(cond, e1, e2) -> StringSet.union (names_expr cond) (StringSet.union (names_expr e1) (names_expr e2))
   (* Conditionnelle -> on extrait les noms de variables de la condition et des branches *)
   | CallE (f::args) -> List.fold_left (fun acc a -> StringSet.union acc (names_expr a)) (names_expr f) args
   (* Appel de fonction -> on extrait les noms de variables de la fonction et des arguments *)


(* Déterminer si fonction est récursive terminale *)
let rec is_tailrec_expr f exp = match exp with
  |IfThenElse (condition, exp1, exp2 )-> not(StringSet.mem f (names_expr condition)) &&    (*StringSet.mem vérifie la présence de f dans l'ensemble créé à partir des variables de la condition*)
                        is_tailrec_expr f exp1 && is_tailrec_expr f exp2
  (* Pas d’appel recursif dans la condition et les appels dans les deux branches sont terminaux*)
  |CallE [] -> true  (*Cas de base*)
  |CallE (expr::liste) -> (not (StringSet.mem f (names_expr expr))) && is_tailrec_expr f (CallE(liste)) 
  (* Cas récursif : pas d'appel récursif de f*)
  |expr -> not (StringSet.mem f (names_expr expr))
  (*Toutes les autres expressions ne doivent pas contenir f*)


(* Effectuer la transformation *)

let transf_expr f listep e = 
  if not (is_tailrec_expr f e) then Return e else
    let rec trad = function
    |IfThenElse (condition, exp1, exp2)-> Cond(condition, trad exp1, trad exp2)
    |CallE (nom::args) -> Assign (listep ,args)
    |exp -> Return exp  
  in While (Const(BoolV true), trad(e)) 


let transf_fpdefn (Fundefn (FPdecl (tpf,nomf, argsliste), exp)) = (Procdefn (FPdecl (tpf,nomf, argsliste), transf_expr nomf (List.map name_of_vardecl argsliste) exp))


let transf_prog (Prog(fdfs, e)) = Prog(List.map transf_fpdefn fdfs, e)

