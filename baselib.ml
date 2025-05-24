open Ast
open Mips

module Env = Map.Make(String)

let types =
  Env.of_list [
    "_add", Func_t (Int_t, [ Int_t ; Int_t ]);
    "_sub", Func_t (Int_t, [ Int_t ; Int_t ]);
    "_mul", Func_t (Int_t, [ Int_t ; Int_t ]);
    "_div", Func_t (Int_t, [ Int_t ; Int_t ]);
    "_mod", Func_t (Int_t, [ Int_t ; Int_t ]);
    "_and", Func_t (Bool_t, [ Bool_t ; Bool_t ]);
    "_or", Func_t (Bool_t, [ Bool_t ; Bool_t ]);
    "_not", Func_t (Bool_t, [ Bool_t ]);
    "int", Int_t;
    "bool", Bool_t;
    "string", String_t;
    "_print_int", Func_t (Int_t, [Int_t]);
    "_print_bool", Func_t (Bool_t, [Bool_t]);
    "_print_string", Func_t (String_t, [String_t]);
    "_read_int", Func_t (Int_t, []);
    "_read_bool", Func_t (Bool_t, []);
    "_read_string", Func_t (String_t, []);
    "_lt", Func_t (Bool_t, [ Int_t ; Int_t ]);
    "_gt", Func_t (Bool_t, [ Int_t ; Int_t ]);
    "_eq", Func_t (Bool_t, [ Int_t ; Int_t ]);
  
  ]

let stdlib =
  [ Label "_add"
  ; Lw (T0, Mem (SP, 4))
  ; Lw (T1, Mem (SP, 0))
  ; Add (V0, T0, T1)
  ; Jr RA
  ; Label "_sub"
  ; Lw (T0, Mem (SP, 4))
  ; Lw (T1, Mem (SP, 0))
  ; Sub (V0, T0, T1)
  ; Jr RA
  ; Label "_mul"
  ; Lw (T0, Mem (SP, 4))
  ; Lw (T1, Mem (SP, 0))
  ; Mul (V0, T0, T1)
  ; Jr RA
  ; Label "_div"
  ; Lw (T0, Mem (SP, 4))
  ; Lw (T1, Mem (SP, 0))
  ; Div (T0, T1)
  ; Mflo V0
  ; Jr RA
  ; Label "_mod"
  ; Lw (T0, Mem (SP, 4))
  ; Lw (T1, Mem (SP, 0))
  ; Div (T0, T1)
  ; Mfhi V0
  ; Jr RA
  ; Label "_and"
  ; Lw (T0, Mem (SP, 4))
  ; Lw (T1, Mem (SP, 0))
  ; And (V0, T0, T1)
  ; Jr RA
  ; Label "_or"
  ; Lw (T0, Mem (SP, 4))
  ; Lw (T1, Mem (SP, 0))
  ; Or (V0, T0, T1)
  ; Jr RA
  ; Label "_not"
  ; Lw (T0, Mem (SP, 0))
  ; Li (T1, 1)
  ; Xor (V0, T0, T1)
  ; Jr RA
  ; Label "_print_int"
  ; Lw (A0, Mem (SP, 0))
  ; Li (V0, 1)
  ; Syscall
  ; Move (V0, A0)   
  ; Jr RA

  ; Label "_print_string"
  ; Lw (A0, Mem (SP, 0))
  ; Li (V0, 4)
  ; Syscall
  ; Move (V0, A0)   
  ; Jr RA

  ; Label "_print_bool"
  ; Lw (A0, Mem (SP, 0))
  ; Li (V0, 1)
  ; Syscall
  ; Move (V0, A0)   
  ; Jr RA

  ; Label "_read_int"
  ; La (A0, "_prompt_int")
  ; Li (V0, 4)           
  ; Syscall
  ; Li (V0, 5)          
  ; Syscall              
  ; Jr RA                

  ; Label "_read_bool"
  ; La (A0, "_prompt_bool")
  ; Li (V0, 4)          
  ; Syscall
  ; Li (V0, 5)          
  ; Syscall            
  ; Jr RA              

  ; Label "_read_string"
  ; La (A0, "_prompt_string")
  ; Li (V0, 4)          
  ; Syscall
  ; La (A0, "_buffer")   
  ; Li (A1, 100)
  ; Li (V0, 8)
  ; Syscall
  ; La (V0, "_buffer")   
  ; Jr RA

  ; Label "_lt"
  ; Lw (T0, Mem (SP, 4))
  ; Lw (T1, Mem (SP, 0))
  ; Slt (V0, T0, T1)
  ; Jr RA
  
  ; Label "_gt"
  ; Lw (T0, Mem (SP, 4))
  ; Lw (T1, Mem (SP, 0))
  ; Slt (V0, T1, T0)
  ; Jr RA
  
  ; Label "_eq"
  ; Lw (T0, Mem (SP, 4))
  ; Lw (T1, Mem (SP, 0))
  ; Seq (V0, T0, T1)
  ; Jr RA
  ]
  
  
