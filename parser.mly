%token Lend
%token <int> Lint
%token <bool> Lbool
%token <string> Lstring
%token Lplus Lminus Ltimes Ldiv Lmod Land Lor Lnot
%token Lparen Rparen
%token Lprint_int Lprint_bool Lprint_string
%token Lread_int Lread_bool Lread_string
%token <string> Lident
%token Leq               (* Assignation "=" *)
%token Lsemicolon        (* Fin d'instruction ";" *)
%token Lreturn
%token Lint_type Lbool_type Lstring_type
%token Lif Lelse Lbrace Rbrace Llt Lgt
%token Lwhile


%left Lor
%left Land
%nonassoc Lnot
%left Lplus Lminus
%left Ltimes Ldiv Lmod
%nonassoc Llt Lgt Leq   (* Priorité des opérateurs de comparaison *)

%start prog
%type <Ast.Syntax.prog> prog

%%

prog:
| i = instr ; p = prog { i :: p }
| Lend { [] }
;

block:
| list = instr* { list }
;

instr:
| Lint_type ; id = Lident ; Lsemicolon {
    Decl { name = id; typ = Int_t; expr = None; pos = $startpos(id) }
}
| Lint_type ; id = Lident ; Leq ; e = expr ; Lsemicolon {
    Decl { name = id; typ = Int_t; expr = Some e; pos = $startpos(id) }
}
| Lbool_type ; id = Lident ; Lsemicolon {
    Decl { name = id; typ = Bool_t; expr = None; pos = $startpos(id) }
}
| Lbool_type ; id = Lident ; Leq ; e = expr ; Lsemicolon {
    Decl { name = id; typ = Bool_t; expr = Some e; pos = $startpos(id) }
}
| Lstring_type ; id = Lident ; Lsemicolon {
    Decl { name = id; typ = String_t; expr = None; pos = $startpos(id) }
}
| Lstring_type ; id = Lident ; Leq ; e = expr ; Lsemicolon {
    Decl { name = id; typ = String_t; expr = Some e; pos = $startpos(id) }
}
| id = Lident ; Leq ; e = expr ; Lsemicolon {
    Assign { name = id; expr = e; pos = $startpos(id) }
}
| Lreturn ; e = expr ; Lsemicolon {
    Return { expr = e; pos = $startpos($1) }
}
| e = expr ; Lsemicolon {
    Return { expr = e; pos = $startpos(e) }
}
| Lif; Lparen; e = expr; Rparen; Lbrace; b1 = block; Rbrace; 
  Lelse; Lbrace; b2 = block; Rbrace {
    If { cond = e; then_block = b1; else_block = Some b2; pos = $startpos($1) }
}
| Lif; Lparen; e = expr; Rparen; Lbrace; b = block; Rbrace {
    If { cond = e; then_block = b; else_block = None; pos = $startpos($1) }
}
| Lwhile; Lparen; e = expr; Rparen; Lbrace; b = block; Rbrace {
      While { cond = e; block = b; pos = $startpos($1) }
  }
;

expr:
| n = Lint { Int { value = n ; pos = $startpos(n) } }
| b = Lbool { Bool { value = b ; pos = $startpos(b) } }
| s = Lstring { String { value = s ; pos = $startpos(s) } }
| id = Lident { Var { name = id; pos = $startpos(id) } }
| a = expr ; Lplus ; b = expr {
    Call { func = "_add"; args = [a; b]; pos = $startpos($2) }
}
| a = expr ; Lminus ; b = expr {
    Call { func = "_sub"; args = [a; b]; pos = $startpos($2) }
}
| a = expr ; Ltimes ; b = expr {
    Call { func = "_mul"; args = [a; b]; pos = $startpos($2) }
}
| a = expr ; Ldiv ; b = expr {
    Call { func = "_div"; args = [a; b]; pos = $startpos($2) }
}
| a = expr ; Lmod ; b = expr {
    Call { func = "_mod"; args = [a; b]; pos = $startpos($2) }
}
| a = expr ; Land ; b = expr {
    Call { func = "_and"; args = [a; b]; pos = $startpos($2) }
}
| a = expr ; Lor ; b = expr {
    Call { func = "_or"; args = [a; b]; pos = $startpos($2) }
}
| Lnot ; a = expr {
    Call { func = "_not"; args = [a]; pos = $startpos($1) }
}
| a = expr; Llt; b = expr { 
    Call { func = "_lt"; args = [a; b]; pos = $startpos($2) }
}
| a = expr; Lgt; b = expr {
    Call { func = "_gt"; args = [a; b]; pos = $startpos($2) }
}
| a = expr; Leq; b = expr {
    Call { func = "_eq"; args = [a; b]; pos = $startpos($2) }
}
| Lparen ; e=expr ; Rparen { e }
| Lprint_int ; Lparen ; e=expr ; Rparen {
    Call { func="_print_int"; args=[e]; pos=$startpos($1) }
}
| Lprint_bool ; Lparen ; e=expr ; Rparen {
    Call { func="_print_bool"; args=[e]; pos=$startpos($1) }
}
| Lprint_string ; Lparen ; e=expr ; Rparen {
    Call { func="_print_string"; args=[e]; pos=$startpos($1) }
}
| Lread_int;Lparen;Rparen {
    Call { func="_read_int"; args=[]; pos=$startpos($1) }
}
| Lread_bool;Lparen;Rparen {
    Call { func="_read_bool"; args=[]; pos=$startpos($1) }
}
| Lread_string;Lparen;Rparen {
    Call { func="_read_string"; args=[]; pos=$startpos($1) }
}
;
