type label = string

type reg =
| V0
| SP
| T0
| T1
| RA
| A0
| A1
| Zero
| FP


type loc =
| Lbl of label
| Mem of reg * int

type instr =
| Nop
| Label of label
| Li of reg * int
| Addi of reg * reg * int
| Add of reg * reg * reg
| Sw of reg * loc
| Lw of reg * loc
| Jal of loc
| Jr of reg
| La    of reg * label
| Move  of reg * reg
| Syscall
| Sub of reg * reg * reg
| Mul of reg * reg * reg
| Div of reg * reg
| Mfhi of reg
| Mflo of reg
| And of reg * reg * reg
| Or of reg * reg * reg
| Xor of reg * reg * reg
| Beqz of reg * loc  
| J of loc           
| Slt of reg * reg * reg  
| Seq of reg * reg * reg  

type directive =
| Asciiz of string

type decl = label * directive

type mips = { text : instr list ; data : decl list }

let fmt_reg = function
  | V0 -> "$v0"
  | SP -> "$sp"
  | T0 -> "$t0"
  | T1 -> "$t1"
  | RA -> "$ra"
  | A0 -> "$a0" 
  | A1 -> "$a1"	
  | Zero -> "$zero"
  | FP -> "$fp"

let fmt_loc = function
  | Lbl l -> l
  | Mem (r, o) -> Printf.sprintf "%d(%s)" o (fmt_reg r)

let fmt_instr = function
  | Nop              -> "  nop"
  | Label l          -> Printf.sprintf "%s:" l
  | Li (r, i)        -> Printf.sprintf "  li %s, %d" (fmt_reg r) i
  | Addi (rd, r, i)  -> Printf.sprintf "  addi %s, %s, %d" (fmt_reg rd) (fmt_reg r) i
  | Add (rd, r1, r2) -> Printf.sprintf "  add %s, %s, %s" (fmt_reg rd) (fmt_reg r1) (fmt_reg r2)
  | Sw (r, l)        -> Printf.sprintf "  sw %s, %s" (fmt_reg r) (fmt_loc l)
  | Lw (r, l)        -> Printf.sprintf "  lw %s, %s" (fmt_reg r) (fmt_loc l)
  | Jal l            -> Printf.sprintf "  jal %s" (fmt_loc l)
  | Jr r             -> Printf.sprintf "  jr %s" (fmt_reg r)
  | La (r, l)        -> Printf.sprintf "  la %s, %s" (fmt_reg r) l 
  | Move (r1, r2)     -> Printf.sprintf "  move %s, %s" (fmt_reg r1) (fmt_reg r2)
  | Syscall -> "  syscall"
  | Sub (rd, r1, r2) -> Printf.sprintf "  sub %s, %s, %s" (fmt_reg rd) (fmt_reg r1) (fmt_reg r2)
  | Mul (rd, r1, r2) -> Printf.sprintf "  mul %s, %s, %s" (fmt_reg rd) (fmt_reg r1) (fmt_reg r2)
  | Div (r1, r2) -> Printf.sprintf "  div %s, %s" (fmt_reg r1) (fmt_reg r2)
  | Mfhi r -> Printf.sprintf "  mfhi %s" (fmt_reg r)
  | Mflo r -> Printf.sprintf "  mflo %s" (fmt_reg r)
  | And (rd, r1, r2) -> Printf.sprintf "  and %s, %s, %s" (fmt_reg rd) (fmt_reg r1) (fmt_reg r2)
  | Or (rd, r1, r2) -> Printf.sprintf "  or %s, %s, %s" (fmt_reg rd) (fmt_reg r1) (fmt_reg r2)
  | Xor (rd, r1, r2) -> Printf.sprintf "  xor %s, %s, %s" (fmt_reg rd) (fmt_reg r1) (fmt_reg r2)
  | Beqz (r, l) -> Printf.sprintf "  beqz %s, %s" (fmt_reg r) (fmt_loc l)
  | J l -> Printf.sprintf "  j %s" (fmt_loc l)
  | Slt (rd, r1, r2) -> Printf.sprintf "  slt %s, %s, %s" (fmt_reg rd) (fmt_reg r1) (fmt_reg r2)
  | Seq (rd, r1, r2) -> Printf.sprintf "  seq %s, %s, %s" (fmt_reg rd) (fmt_reg r1) (fmt_reg r2)



let fmt_dir = function
  | Asciiz s -> Printf.sprintf ".asciiz \"%s\"" s

  let emit oc asm =
    Printf.fprintf oc ".text\n.globl main\n" ;
    List.iter (fun i -> Printf.fprintf oc "%s\n" (fmt_instr i)) asm.text ;
    Printf.fprintf oc "\n.data\n" ;
    Printf.fprintf oc "_buffer: .space 100\n" ;
    Printf.fprintf oc "_prompt_int: .asciiz \"Veuillez entrer un entier : \"\n" ;
    Printf.fprintf oc "_prompt_bool: .asciiz \"Veuillez entrer un booleen (0/1) : \"\n" ;
    Printf.fprintf oc "_prompt_string: .asciiz \"Veuillez entrer une chaine : \"\n" ;
    List.iter (fun (l, d) -> Printf.fprintf oc "%s: %s\n" l (fmt_dir d)) asm.data
  
