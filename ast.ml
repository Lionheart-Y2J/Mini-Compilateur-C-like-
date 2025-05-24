type base_t =
| Int_t
| Bool_t
| String_t
| Func_t of base_t * base_t list


let rec string_of_t = function
| Int_t -> "int"
| Bool_t -> "bool"
| String_t -> "string"
| Func_t (rt, ats) ->
  Printf.sprintf "(%s) -> %s"
    (String.concat ", " (List.map string_of_t ats))
    (string_of_t rt)

module Syntax = struct
  type expr =
    | Int    of { value: int; pos: Lexing.position }
    | Bool   of { value: bool; pos: Lexing.position }
    | String of { value: string; pos: Lexing.position }
    | Var    of { name: string; pos: Lexing.position }
    | Call   of { func: string; args: expr list; pos: Lexing.position }

  type instr =
    | Decl   of { name: string; typ: base_t; expr: expr option; pos: Lexing.position }
    | Assign of { name: string; expr: expr; pos: Lexing.position }
    | Return of { expr: expr; pos: Lexing.position }
    | If     of { cond: expr; then_block: block; else_block: block option; pos: Lexing.position }
    | While  of { cond: expr; block: block; pos: Lexing.position }


  and block = instr list

  type prog = instr list
end

module IR = struct
  type expr =
    | Int    of int
    | Bool   of bool
    | String of string
    | Var    of string  
    | Call   of string * expr list


  type instr =
    | Decl   of string * base_t * expr option
    | Assign of string * expr
    | Return of expr
    | If     of expr * block * block option
    | While  of expr * block

  
  and block = instr list

  type prog = instr list
end
