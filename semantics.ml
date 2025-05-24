open Ast
open Ast.IR
open Baselib

exception Error of string * Lexing.position

(* Récupère la position d'une expression pour les messages d'erreur *)
let expr_pos expr =
  match expr with
  | Syntax.Int n -> n.pos
  | Syntax.Bool b -> b.pos
  | Syntax.String s -> s.pos
  | Syntax.Var v -> v.pos
  | Syntax.Call c -> c.pos

(* lève une erreur de type *)
let errt expected given pos =
  raise (Error (Printf.sprintf "expected %s but given %s"
                  (string_of_t expected)
                  (string_of_t given),
                pos))

(* analyse une expression *)
let rec analyze_expr env expr =
  match expr with
  | Syntax.Int n -> Int n.value, Int_t
  | Syntax.Bool b -> Bool b.value, Bool_t
  | Syntax.String s -> String s.value, String_t
  | Syntax.Var v ->     (* gestion des variables, on vérifie si elles existent dans l'environnement *)
    (match Env.find_opt v.name env with
     | None -> raise (Error (Printf.sprintf "unbound variable '%s'" v.name, v.pos))
     | Some t -> Var v.name, t)
  (* ci-dessous gestion des appels de fonction *)
  | Syntax.Call c ->
    match Env.find_opt c.func env with
    | None -> raise (Error (Printf.sprintf "unknown function '%s'" c.func, c.pos))  
    | Some (Func_t (rt, ats)) -> 
      if List.length c.args <> List.length ats then        (* vérification de la compatibilité entre les arguments donnés et attendus *)
        raise (Error (Printf.sprintf "arity mismatch for function '%s': expected %d arguments but given %d"
                        c.func (List.length ats) (List.length c.args), c.pos));
      let aas = List.fold_left2
        (fun aas at a ->
          let aa, aat = analyze_expr env a in
          if at <> aat then errt at aat (expr_pos a)
          else aa :: aas)
        [] ats c.args
      in Call (c.func, List.rev aas), rt
    | Some _ ->
      raise (Error (Printf.sprintf "not a function '%s'" c.func, c.pos))


let rec analyze_instr env instr =
  (* gestion des cas comme "int a = b + c;" ici *)
  match instr with
  | Syntax.Decl d ->
    if Env.mem d.name env then
      raise (Error (Printf.sprintf "variable '%s' already declared" d.name, d.pos));
    let t, e_opt =
      match d.expr with
      | Some e ->
          let ae, at = analyze_expr env e in
          if at <> d.typ then errt d.typ at (expr_pos e);
          at, Some ae
      | None -> d.typ, None
    in
    Env.add d.name t env, Decl (d.name, t, e_opt)
  | Syntax.Assign a ->     (* affectation classique, on vérifie si la variable existe et si le type est bon *)
    let ae, at = analyze_expr env a.expr in
    (match Env.find_opt a.name env with
     | None -> raise (Error (Printf.sprintf "unbound variable '%s'" a.name, a.pos))
     | Some t when t <> at -> raise (Error ("type mismatch in assignment", a.pos))
     | Some _ -> env, Assign (a.name, ae))
  | Syntax.Return r ->     (* retour d'une valeur dans une fonction *)
    let ae, _ = analyze_expr env r.expr in
    env, Return ae
  | Syntax.If i ->     (* conditionnelle "if", avec une éventuelle branche "else" *)
    let cond, cond_t = analyze_expr env i.cond in
    if cond_t <> Bool_t then
      raise (Error ("condition must be a boolean", expr_pos i.cond));
    let then_block = analyze_prog env i.then_block in
    let else_block = match i.else_block with
      | Some b -> Some (analyze_prog env b)
      | None -> None
    in
    env, If (cond, then_block, else_block)
  
    | Syntax.While w ->     (* boucle "while" classique *)

      let cond, cond_t = analyze_expr env w.cond in
      if cond_t <> Bool_t then
        raise (Error ("condition must be a boolean", expr_pos w.cond));
      let block = analyze_prog env w.block in
      env, While (cond, block)



and  analyze_prog env prog = (* analyse un programme entier *)
  match prog with
  | [] -> []
  | i :: p ->
    let env', ai = analyze_instr env i in
    ai :: analyze_prog env' p


let analyze prog = (* point d'entrée principal pour analyser un programme complet *)
  analyze_prog Baselib.types prog
