## Mini compilateur langage C-like

Ce readme présentera mon projet de compilateur de langage "similaire" au C vers de l'assembleur MIPS.Je précise que j'ai repris depuis le dossier proj2 que vous avez envoyé sur mattermost. Ce compilateur permet actuellement la génération de code MIPS valide à partir de fichiers test contenant des instructions et actuellemen,j'ai implémenté les types de bases,et leur opérations  associées,les appels de fonction étaient déjà la via le Call,et j'ai également ajouté la déclaration et assignation de variables ainsi que le branchement conditionnel if et la boucle while avec des blocs (bien que dans des cas particuliers que je mentionnerais,il ne marchent pas)

Il manque donc à ce projet les définitions de fonctions,pour qu'il soit complet pour faire une sorte de mini-C.J'aurais pu me donner plus de moyens sur le 2 derniers jours d'aller plus loin et de finir le projet au bout,mais au point actuel on ne peut tester que des instructions.

Dans ce rapport on présentera ce que font nos fichiers,avec des bouts de code si nécéssaire (je vais essayer de pas trop en mettre pour pas polluer le readme),pour comprendre chaque étape du lexer au MIPS.


## Compiler le projet

Mais d'abord pour le tester:

-> dune build main.exe 
-> ./main tests/prog-complet.test > test.s 
-> spim 
-> load "test.s"
-> run 

Et voilà.

## ast.ml : La définition de la structure syntaxique

Le fichier ast.ml définit la structure syntaxique du langage, c'est-à-dire la manière dont les programmes sont représentés en interne après leur analyse syntaxique. Il sert de base pour toutes les étapes suivantes du compilateur : analyse sémantique, génération de code intermédiaire, et production du code MIPS.

 On trouve d'ailleurs deux modules dans ce fichier: Syntax et IR. Le module Syntax représente l'arbre syntaxique abstrait tel qu'il est construit par le parser (via le %type <Ast.Syntax.prog> ). Le module IR  est une version simplifiée de cet arbre qui sera utilisée par le compilateur pour générer le code MIPS.

 un extrait:

 type base_t =
| Int_t
| Bool_t
| String_t
| Func_t of base_t * base_t list

évidemment le base_t qui est là pour représenter chaque type de base ainsi que le type fonctionnel,une fonction avec type de retour et une liste de types pour ses arguments.


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
end

On a notre type expr (types d'expressions) pour représenter les int,les chaines ou les bools,les variables ou les appels de fonctions et de l'autre côté le type instr avec toutes les instructions,déclaration de variables,assignation,retour de valeur, structure conditionnelle "If" et "While" avec leur deux blocs interne. Et bien sûr la structure block qui est une liste d'instructions. Simple jusque là,c'est pas ici que les galères vont commencer


##  lexer.mll : L'analyseur lexical

Le fichier lexer.mll  sert à transformer le code source écrit dans le "pseudo-C" en une séquence de tokens, qui sont des unités lexicales compréhensibles par le parser. Ces tokens représentent les mots-clés, les opérateurs, les identifiants, les littéraux, etc.

Un court extrait du lexer ici:

let num = ['0' - '9']+
let bool = "true" | "false"
let string = '"' [^'"']* '"'
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

Ici on définit les motifs littéraux,qui servent à reconnaitre des chiffres,les mots clés true et false
une chaine de caractère entourée de guillemets. Alpha qui reconnait une majuscule,ident qui lui reconnait identifiant commencant par une lettre suivi de de lettres,chiffres ou soulignés

rule token = parse
| "read_int" { Lread_int }
| "read_bool" { Lread_bool }
| "read_string" { Lread_string }
| '+' { Lplus }
| '-' { Lminus }

Ici on définit les règles,c'est là qu'on reconnait les mots clés,opérateurs... C'est là qu'on définit quels sont les termes qu'on utilise pour notre langage au final.


## parser.mly : L'analyseur syntaxique

Le fichier parser.mly est l'analyseur syntaxique du compilateur. Il définit les règles de grammaire du langage implémenté, en associant chaque règle à une construction de l'AST. On y déclare les tokens qu'on a défini dans le lexer.On y définit nos règles.

Exemple:

%start prog
%type <Ast.Syntax.prog> prog

prog:
| i = instr ; p = prog { i :: p }
| Lend { [] }
;

la directive %start prog indique que la production de départ du programme est prog. En grol'analyseur syntaxique va commencer par essayer d'analyser un programme complet, qui est représenté par la règle prog.


instr:
| Lif; Lparen; e = expr; Rparen; Lbrace; b1 = block; Rbrace;
  Lelse; Lbrace; b2 = block; Rbrace {
    If { cond = e; then_block = b1; else_block = Some b2; pos = $startpos($1) }
}
| Lif; Lparen; e = expr; Rparen; Lbrace; b = block; Rbrace {
    If { cond = e; then_block = b; else_block = None; pos = $startpos($1) }
}

Ces règles définissent la structure du branchement conditionnel if/else. Elles construisent un nœud If dans l'AST avec une condition (cond), un bloc pour le cas vrai (then_block), et un bloc optionnel pour le cas faux (else_block).



(je vais essayer d'accélerer parce qu'il me reste plus trop de temps pour rendre désolé)


## baselib.ml : La bibliothèque de base

Le fichier baselib.ml définit les fonctions et opérations primitives langage qu'on implémente. Ces primitives incluent les opérations arithmétiques, logiques, et les fonctions d'entrée/sortie (print, read, etc.). Elles sont directement traduites en instructions MIPS et utilisées par le compilateur pour générer du code.

On  utilise le module Env pour définir un environnement où chaque fonction ou type de variable est associé à une clé (l'env de baselib sera utilisé d'ailleurs dans l'analyse semantique et la compilation forcément)

Voici comment on définit nos primitives:

let types = Env.of_list [
  "_add", Func_t (Int_t, [ Int_t ; Int_t ]);
  "_sub", Func_t (Int_t, [ Int_t ; Int_t ]);
  "_mul", Func_t (Int_t, [ Int_t ; Int_t ]); 
  ... 
  "_or", Func_t (Bool_t, [ Bool_t ; Bool_t ]);
  "_not", Func_t (Bool_t, [ Bool_t ]);]

  On indique le type de retour de chaque fonction et les types des arguments attendus.

  Nos fonctions mips sont elles définies dans la stdlib

  let stdlib =
  [ Label "_add"
  ; Lw (T0, Mem (SP, 4))
  ; Lw (T1, Mem (SP, 0))
  ; Add (V0, T0, T1)
  ; Jr RA 
  ... ]

 Voilà l'opération + en MIPS et elle sera représentée par _add dans le code généré 

 ## mips.ml : Génération du code MIPS

mips.ml sert à définir les structures nécessaires pour représenter le code MIPS dans le compilateur et à formater ce code en une sortie lisible quand on utilise SPIM. On l'a déjà vu pendant nos TP donc je vais pas me trop rester dessus,voici les directives mips que j'ai ajouté:

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

On a eu l'occasion d'en ajouter certain à l'occasion d'anciens TP donc je les ai repris.
A noter que j'ai défini dans le emit un:  
Printf.fprintf oc "_buffer: .space 100\n" ; pour les affichages de string
ainsi que des prompts pour la saisi via les fonction read.

Ah et j'y ai aussi ajouté d'autres registres
| A0
| A1
| Zero
| FP (Le frame pointer est obligatoire pour les variables!!)

## semantics.ml : Analyseur sémantique

semantics.ml est chargé de vérifier la validité sémantique du programme. Donc ça va inclure la vérification des types, la gestion des variables, et le respect des règles du langage. L'analyseur sémantique agit comme une couche de validation entre le parsing et la génération de code.

On y utilise plusieurs fonctions d'analyse d'expression,d'instruction ou de prog

let rec analyze_expr env expr =
  match expr with
  | Syntax.Int n -> Int n.value, Int_t
  | Syntax.Bool b -> Bool b.value, Bool_t
  | Syntax.String s -> String s.value, String_t
  | Syntax.Var v ->
    (match Env.find_opt v.name env with
     | None -> raise (Error (Printf.sprintf "unbound variable '%s'" v.name, v.pos))
     | Some t -> Var v.name, t)
     ...

Dans la fonction analyze_expr, on fait un match pour analyser les différentes formes possibles d'expressions dans le programme et vérifier leur validité:
|Syntax.Int n: Si l'expression est un entier, on retturn sa valeur et son type Int_t.
|Syntax.Bool b: Si l'expression est un booléen, on return sa valeur et son type Bool_t.
|Syntax.String s: Si l'expression est une chaîne, on return sa valeur et son type String_t.
|Syntax.Var v: Si l'expression est une variable, on vérifie dans l'env si elle a été déclarée. Si elle n'existe pas on va lever une erreur et sinon on return son nom et son type.
|Syntax.Call c : Si l'expression est un appel à une fonction:
-On vérifie que la fonction existe dans l'environnement.
-Ensuite on regarde sile nombre d'arguments donnés correspond au nombre attendu 
-Puis on vérifie que chaque argument est du bon type en les analysant analyze_expr,dont voici un court extrait d'ailleurs.

 | Syntax.While w ->     (* boucle "while" classique *)

      let cond, cond_t = analyze_expr env w.cond in
      if cond_t <> Bool_t then
        raise (Error ("condition must be a boolean", expr_pos w.cond));
      let block = analyze_prog env w.block in
      env, While (cond, block)

Il provient du match sur le cas while,on analyse la condition avec analyze_expr. Puis on vérifie si la condition est un booléen,puis on analyze récursivement le bloc de la boucle (directement avec analyze_prog qui vérifié analyse dans un cas non vide (i :: p) la première instruction i et continue avec le reste p après avoir ajouté l'instruction déjà analysée dans une liste)



## compiler.ml : Compilation vers MIPS

Lcompiler.ml est chargé de transformer le programme source en code MIPS. On y gère la compilation des expressions, des instructions, des blocs, et du programme entier. Et c'était assez difficile comme partie,parce que j'ai du faire pas mal de bricolage vu que je me suis enfermé dans des solutions pas hyper élégantes,et avec tout ce qu'il y'avait à gérer notamment la pile,ça fait quelque chose de moche,mais qui marche (relativement).

Mais pour l'heure ce qu'il faut retenir sur le compiler.ml c'est ça:

On utilise le type var_info est utilisé pour stocker les informations sur les variables :

type var_info = {
  offset: int;        (* Position de la variable dans la pile *)
  typ: Ast.base_t;    (* Type de base de la variable (Int_t, Bool_t, String_t) *)
}

En MIPS, les variables sont stockées sur la pile lors de l'exécution d'un programme. Le système de gestion des variables utilise l'offset, c-à-d la position relative de chaque variable dans la pile. Du coup chaque variable a un offset qui permet de localiser sa valeur sur la pile en fonction de la profondeur de son activation dans la pile d'exécution. Ca va permettre d'optimiser l'accès à la mémoire lors de l'exécution des instructions, car le registre SP (Stack Pointer) pointe vers le sommet de la pile, et les valeurs sont accessibles via des déplacements relatifs à ce registre.
Voila pourquoi j'applique un offset quand je déclare dans compile_instr.


La fonction compile_expr transforme une expression en une séquence d'instructions MIPS. On fait un match pour traiter chaque type d'expression :

let rec compile_expr env expr =
  match expr with
  | Int n -> [ Li (V0, n) ]  (* Charger un entier dans $v0 *)
  | Bool b -> [ Li (V0, if b then 1 else 0) ]  (* Charger un booléen dans $v0 *)
  | String s ->
    let label = generate_unique_label () in
    data := (label, Asciiz s) :: !data;  (* Ajouter la chaîne à la section .data *)
    [ La (V0, label) ]  (* Charger l'adresse de la chaîne dans $v0 *)
  | Var v -> (
      match Env.find_opt v env with
      | None -> failwith (Printf.sprintf "Unbound variable '%s'" v)
      | Some info -> [ Lw (V0, Mem (SP, info.offset)) ]  (* Charger la valeur depuis la pile *)
    )

Les commentaires feront les explications pour moi.

Il y'a ça que j'ai pas commenté:
| Call (func, args) ->
      List.concat_map
        (fun a -> compile_expr env a @ [ Addi (SP, SP, -4); Sw (V0, Mem (SP, 0)) ])
        args
      @ [ Jal (Lbl func); Addi (SP, SP, 4 * List.length args) ]


En gros, il prépare les arguments pour l'appel de fonction, fait l'appel, puis nettoie la pile après l'appel.


On a beaucoup de cas comme:
| Call ("_add", [a; b]) ->
    let ca = compile_expr env a in
    let cb = compile_expr env b in
    ca @ [ Move (T0, V0) ] @
    cb @ [ Move (T1, V0) ] @
    [ Add (V0, T0, T1) ]

Pour chaque opérateur du baselib qui servent à gérer les cas ou on veut faire des opérations avec deux variables comme int a = b + c;
sinon j'arrivais pas à assigner l'opération de deux variable à une autre (Maintenant ça marche,sauf dans le cas précis du and et or pendant un if,pour une raison que j'ai pas encoré trouvé,il ne semble vouloir analyser que le premier argument du if et ignore le deuxième après un && ou ||,je ne m'y suis pas encore plongé dedans pour le résoudre).


La fonction compile_instr transforme une instruction en une séquence d'instructions MIPS. Encore un petit extrait :

| Decl (name, typ, Some expr) ->
    let ce = compile_expr env expr in
    let offset = -8 * (Env.cardinal env + 1) in
    ce @ [ Sw (V0, Mem (SP, offset)) ], Env.add name { offset = offset; typ = typ } env

On  compile une déclaration de variable avec initialisation, en générant le code machine pour l'expression, puis en stockant la valeur dans la pile à un emplacement calculé avec l'offset. L'environnement est mis à jour pour inclure la variable avec son offset et son type.


ou le très casse tête:

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



On gère l'instruction Return expr en compilant l'expression de retour et en ajoutant des instructions spécifiques selon le type de l'expression. Si l'expression est un appel à une fonction comme _read_string, il modifie le comportement du syscall pour retourner correctement une chaîne. Pour certains appels de fonction (_print_int, _print_string, etc.), il évite les syscalls inutiles. Si l'expression est une variable, il gère son type et effectue un syscall approprié pour l'affichage. En fin de compte, il retourne le code compilé et l'environnement mis à jour. J'ai du faire des cas pour une fonction en particulier parce que mon code générait pas toujours le bon syscall. Et c'était très,très barbant,vu que ça marchait souvent pas.

On aura évidemment la même gestion pour if,while ou assign,mais j'ai plus le temps donc j'en parlerai pas,en attendant la compilation complète:

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


a fonction compile_prog compile récursivement une liste d'instructions en générant le code correspondant à chaque instruction via compile_instr. À chaque étape, elle met à jour l'environnement  et continue la compilation pour le reste du programme. Ensuite la fonction compile initialise l'environnement de données, compile le programme entier, puis assemble le code final en ajoutant des instructions pour l'entrée du programme (le main), avant de retourner le code compilé avec les données.

Et voilà.

Il ne reste qu'a tester via les fichiers de test que j'ai laissé dans le dossier tests. On aura la génération du code MIPS et vous pourrez tester dans le fichier .s vide test.s

Voila la fin de mon rapport,ce projet aura été une aventure pour le coup,et je pensais pas m'approcher de la fin même avec le temps que j'ai mis dessus mais je suis satisfait et fier du résultat même si il n'a pas abouti avec les méthoes les plus optimales.






J'ai pas parlé du fichier main.ml car je ne l'ai pas touché et autant ne pas faire de paragraphe la dessus mais ce qu'il fait lire un fichier avec Lexing, l'analyse syntaxiquement avec le parser, puis fais l'analyse sémantique. Ensuite, il compile et génère du code assembleur. Mais oui il est super important car on peut rien lancer sans.









