open Ast.IR
open Mips

type var_info = {
  offset: int;
  typ: Ast.base_t;
}
(* Structure contenant des infos sur une variable : position dans la pile et type de base. *)

module Env = Map.Make(String)

(* Compteur pour générer des labels uniques. *)
let label_counter = ref 0
let generate_unique_label () =
  incr label_counter;
  Printf.sprintf "_string_%d" !label_counter

let data = ref []

(*  ci dessous gestion des expressions en plus des cas ou on veut faire des des opérations
avec deux variables comme int a = b + c;
c'est pas très élégant mais ça fonctionne (excepté or et and dans des if) *)
let rec compile_expr env expr =
  match expr with
  | Int n -> [ Li (V0, n) ] (* Charger un entier dans V0 *)
  | Bool b -> [ Li (V0, if b then 1 else 0) ] (* Charger un booléen dans V0 *)
  | String s ->
    let label = generate_unique_label () in
    data := (label, Asciiz s) :: !data; (* ajouter la chaîne à la section .data *)
    [ La (V0, label) ] (* charger l'adresse de la chaîne dans V0 *)
  | Var v -> (
      match Env.find_opt v env with
      | None -> failwith (Printf.sprintf "Unbound variable '%s'" v)
      | Some info -> [ Lw (V0, Mem (SP, info.offset)) ]
    )
    (* Gestion des appels natifs comme l'addition ou la soustraction. *)
  | Call ("_add", [a; b]) ->
    let ca = compile_expr env a in
    let cb = compile_expr env b in
    ca @ [ Move (T0, V0) ] @  (* sauvegarder le premier opérande dans T0 *)
    cb @ [ Move (T1, V0) ] @  (* sauvegarder le second opérande dans T1 *)
    [ Add (V0, T0, T1) ]      (* additionner les deux valeurs,on fait à peu près pareil
                                par la suite pour chaque opération,selon les cas  *)

  | Call ("_sub", [a; b]) ->
    let ca = compile_expr env a in
    let cb = compile_expr env b in
    ca @ [ Move (T0, V0) ] @
    cb @ [ Move (T1, V0) ] @
    [ Sub (V0, T0, T1) ]
  | Call ("_mul", [a; b]) ->
    let ca = compile_expr env a in
    let cb = compile_expr env b in
    ca @ [ Move (T0, V0) ] @
    cb @ [ Move (T1, V0) ] @
    [ Mul (V0, T0, T1) ]
  | Call ("_div", [a; b]) ->
    let ca = compile_expr env a in
    let cb = compile_expr env b in
    ca @ [ Move (T0, V0) ] @
    cb @ [ Move (T1, V0) ] @
    [ Div (T0, T1); Mflo V0 ]
  | Call ("_mod", [a; b]) ->
    let ca = compile_expr env a in
    let cb = compile_expr env b in
    ca @ [ Move (T0, V0) ] @
    cb @ [ Move (T1, V0) ] @
    [ Div (T0, T1); Mfhi V0 ]

  | Call ("_not", [a]) ->
    let ca = compile_expr env a in
    ca @ [ Li (T1, 1); Xor (V0, V0, T1) ]

  | Call ("_lt", [a; b]) ->
    let ca = compile_expr env a in
    let cb = compile_expr env b in
    ca @ [ Move (T0, V0) ] @  
    cb @ [ Move (T1, V0) ] @  
    [ Slt (V0, T0, T1) ]      

  | Call ("_gt", [a; b]) ->
    let ca = compile_expr env a in
    let cb = compile_expr env b in
    ca @ [ Move (T0, V0) ] @  
    cb @ [ Move (T1, V0) ] @  
    [ Slt (V0, T1, T0) ]   

  | Call ("_eq", [a; b]) ->
      let ca = compile_expr env a in
      let cb = compile_expr env b in
      ca @ [ Move (T0, V0) ] @ 
      cb @ [ Move (T1, V0) ] @  
      [ Seq (V0, T0, T1) ]     
  
  | Call ("_or", [a; b]) ->
    let ca = compile_expr env a in
    let cb = compile_expr env b in
    ca @ [ Move (T0, V0) ] @  
    cb @ [ Move (T1, V0) ] @  
    [ Or (V0, T0, T1) ]      
    
  | Call ("_and", [a; b]) ->
      let ca = compile_expr env a in
      let cb = compile_expr env b in
      ca @ [ Move (T0, V0) ] @  
      cb @ [ Move (T1, V0) ] @  
      [ And (V0, T0, T1) ]      
      
        (* Les deux coupables add et or ne marchent pas pour les if quand on compare deux variables
          je crois qu'il n'évaluent que la première comparaison du if et ignore la deuxième mais je ne me
            suis pas penché dessus encore *)
    
    | Call (func, args) ->
      List.concat_map
        (fun a -> compile_expr env a @ [ Addi (SP, SP, -4); Sw (V0, Mem (SP, 0)) ])
        args
      @ [ Jal (Lbl func); Addi (SP, SP, 4 * List.length args) ]



    
    
(* Compilation des instructions *)
let rec compile_instr env instr =
  match instr with
  | Decl (name, typ, Some expr) ->
      let ce = compile_expr env expr in
      let offset = -8 * (Env.cardinal env + 1) in
      ce @ [ Sw (V0, Mem (SP, offset)) ],
      Env.add name { offset = offset; typ = typ } env
  | Decl (name, typ, None) ->
      let offset = -8 * (Env.cardinal env + 1) in
      [ Li (V0, 0); Sw (V0, Mem (SP, offset)) ],
      Env.add name { offset = offset; typ = typ } env
  | Assign (name, expr) ->
    let ce = compile_expr env expr in
    let info =
      match Env.find_opt name env with
      | None -> failwith ("Variable not found: " ^ name)
      | Some info -> info
    in 
    ce @ [ Sw (V0, Mem (SP, info.offset)) ], env
| If(cond, then_block, else_block_opt) ->
  let else_label = generate_unique_label () in
  let end_label = generate_unique_label () in
  let cond_code = compile_expr env cond in
  let then_code = compile_block env then_block in
  let else_code = match else_block_opt with
    | Some else_block -> compile_block env else_block
    | None -> []
  in
  cond_code @
  [ Beqz (V0, Lbl else_label) ] @
  then_code @
  [ J (Lbl end_label); Label else_label ] @
  else_code @
  [ Label end_label ], env

  | While(cond, block) ->
    let begin_label = generate_unique_label () in
    let end_label = generate_unique_label () in
    let cond_code = compile_expr env cond in
    let block_code = compile_block env block in
    [ Label begin_label ] @
    cond_code @
    [ Beqz (V0, Lbl end_label) ] @
    block_code @
    [ J (Lbl begin_label);
      Label end_label ], env
    
  | Return expr ->
      let ce = compile_expr env expr in
      ce @ (match expr with
            | Call (func, _) when func = "_read_string" -> (* Pas joli... mais ça garantit un bon retour de string quand on appelle read *)
              [ Move (A0, V0);
                Li (V0, 4);    (* pour ne pas avoir le syscall 1,au risque de retourner l'adresse mémoire du string plutot que le string *)
                Syscall ]
            | Call (func, _) when (func = "_print_int" || func = "_print_string" || func = "_print_bool") ->
                []  (*vu que les print affichent le contenu spécifié,
                     on fait ça pour éviter un syscall et un affichage double inutile*)
            | String _ ->  
                [ Move (A0, V0);
                  Li (V0, 4);
                  Syscall ] (* si c'est un string on affiche*)
            | Var v ->
              (* si c'est une variable ont détermine son type pour faire leb on syscall*)
                (match Env.find_opt v env with
                 | Some info -> 
                     [ Move (A0, V0);
                       (match info.typ with
                        | Ast.String_t -> Li (V0, 4)
                        | _ -> Li (V0, 1));
                       Syscall ]
                 | None -> failwith ("Variable not found: " ^ v))
            | _ -> 
                [ Move (A0, V0);
                  Li (V0, 1);
                  Syscall ]), env

      and compile_block env blk =
        List.concat_map (fun i -> let code, env' = compile_instr env i in code) blk

    


(* compilation d'un programme complet *)
let rec compile_prog env prog =
  match prog with
  | [] -> []
  | i :: p ->
    let ci, new_env = compile_instr env i in
    ci @ compile_prog new_env p

let compile prog =
  data := [];
  let compiled_prog = compile_prog Env.empty prog in
  { text =
      Baselib.stdlib @ [ Label "main"; Addi (SP, SP, -4); Sw (RA, Mem (SP, 0)) ]
      @ compiled_prog @ [ Lw (RA, Mem (SP, 0)); Addi (SP, SP, 4); Jr RA ];
    data = List.rev !data }

