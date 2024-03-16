open PPrint
open Lang


(* Global constant *)
let indent_level = 4

let doc_of_const = function
    BoolV true -> string "True"
  | BoolV false -> string "False"
  | IntV i -> PPrint.OCaml.int i
  | _ -> empty
    
let doc_of_var v = string v

let doc_of_var_list vs = parens (separate_map comma string vs)

(* TODO: to be completed *)
let doc_of_barith ba = string "+"

(* TODO: to be completed *)
let doc_of_bcompar bc = string "=="

let doc_of_blogic = function
    BLand -> string "and"
  | BLor -> string "or"
              
let doc_of_binop = function
    BArith ba -> doc_of_barith ba
  | BCompar bc -> doc_of_bcompar bc
  | BLogic bl -> doc_of_blogic bl

let rec doc_of_expr = function
    Const c -> doc_of_const c
  | VarE v -> doc_of_var v
  | BinOp (bop, e1, e2) -> failwith "TODO"
  (* See https://docs.python.org/3/reference/expressions.html#conditional-expressions *)
  | IfThenElse(cond, t, e) -> failwith "TODO"
  | CallE(VarE(fn)::es) ->
    doc_of_var fn ^^ doc_of_expr_list es
  | CallE(_) -> empty
and doc_of_expr_list es = parens (separate_map comma doc_of_expr es)
    
(* TODO: to be completed *)
let rec doc_of_cmd = function
    Skip -> empty
  | Assign(vs, es) -> doc_of_var_list vs ^^ equals ^^ doc_of_expr_list es
  | Seq(c1, c2) -> doc_of_cmd c1 ^^ doc_of_cmd c2
  | Cond(e, c1, c2) ->
    (separate space [string "if"; doc_of_expr e; colon]) ^^ 
    (nest indent_level (hardline ^^ (doc_of_cmd c1))) ^^
    (match c2 with
       Skip -> empty
     | _ ->
       hardline ^^
       (separate space [string "else"; colon]) ^^ 
       (nest indent_level (hardline ^^ (doc_of_cmd c2))))
  | While(e, c) ->   failwith "TODO"
  | Return(e) ->   failwith "TODO"


(* TODO: to be completed *)
let doc_of_fpdefn  = function
    Fundefn(FPdecl(rt, fn, params), e)->
      (separate space [string "def"; string fn;
      (doc_of_var_list (List.map name_of_vardecl params));
      colon]) ^^
      (nest indent_level (hardline ^^ 
                          (string "return"  ^^ parens (doc_of_expr e))))
| Procdefn(FPdecl(rt, fn, params), c) -> failwith "TODO"

let doc_of_prog (Prog(fdfs, e)) =
  (separate_map hardline doc_of_fpdefn fdfs) ^^
  hardline ^^
  (string "print" ^^ (parens (string "str" ^^ (parens (doc_of_expr e))))) ^^
  hardline
  
let print_prog prg =
  ToChannel.pretty 0.5 80 stdout (doc_of_prog prg);
  flush stdout
    
