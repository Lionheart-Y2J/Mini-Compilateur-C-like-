{
  open Parser
  
  exception Error of char
}

let num = ['0' - '9']+
let bool = "true" | "false"
let string = '"' [^'"']* '"'
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*



rule token = parse
| eof { Lend }
| [' ' '\t']+ { token lexbuf }
| '\n' { Lexing.new_line lexbuf ; token lexbuf }
| "print_int" { Lprint_int }
| "print_bool" { Lprint_bool }
| "print_string" { Lprint_string }
| "read_int" { Lread_int }
| "read_bool" { Lread_bool }
| "read_string" { Lread_string }
| '+' { Lplus }
| '-' { Lminus }
| '*' { Ltimes }
| '/' { Ldiv }
| '%' { Lmod }
| "&&" { Land }
| "||" { Lor }
| "!" { Lnot }
| '(' { Lparen }
| ')' { Rparen }
| "if"   { Lif }
| "else" { Lelse }
| "{"    { Lbrace }
| "}"    { Rbrace }
| "<"    { Llt }
| ">"    { Lgt }
| "=="   { Leq }
| "while" { Lwhile }
| "int" { Lint_type }
| "bool" { Lbool_type }
| "str" { Lstring_type }
| "return" { Lreturn }
| '='        { Leq }        
| ';'        { Lsemicolon }  
| ident as id {
    if id = "true" || id = "false" then Lbool (bool_of_string id)
    else Lident id            (* identifiant de variable *)
}
| num as n { Lint (int_of_string n) } 
| string as s { Lstring (String.sub s 1 (String.length s - 2)) } 
| _ as c { raise (Error c) }

