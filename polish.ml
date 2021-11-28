
(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(*****************************************************************************)
(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
type position = int

(** Nom de variable *)
type name = string

(** Les mots-clefs *)
type keywords = 
| READ    of string
| IF      of string
| ELSE    of string
| PRINT   of string
| COMMENT of string
| WHILE   of string


(** Opérateurs arithmétiques : + - * / % *)
type op = Add | Sub | Mul | Div | Mod

(** Expressions arithmétiques *)
type expr =
  | Num of int
  | Var of name
  | Op of op * expr * expr

(** Opérateurs de comparaisons *)
type comp =
| Eq (* = *)
| Ne (* Not equal, <> *)
| Lt (* Less than, < *)
| Le (* Less or equal, <= *)
| Gt (* Greater than, > *)
| Ge (* Greater or equal, >= *)

(** Condition : comparaison entre deux expressions *)
type cond = expr * comp * expr

(** Instructions *)
type instr =
  | Set of name * expr    (* := *)
  | Read of name
  | Print of expr
  | If of cond * block * block
  | While of cond * block
and block = (position * instr) list

(** Un programme Polish est un bloc d'instructions *)
type program = block

type sign = Neg | Zero | Pos | Error

module NameTable = Map.Make(String)

module NameSet   = Set.Make(String)

(**********************  Syntax check up  *******************************)

let check_variable (st : int * (string list)) : bool =  
   failwith "TODO" 
 
 let check_assignment (st : (int * string) list) : bool =  
   failwith "TODO"

(********************** Auxiliary functions *****************************)

(* read code from file and store it to list *)
let read_file (filename:string) : ((int * string) list) =  
  let chan = Stdlib.open_in filename in
  let try_read () =
    try Some (input_line chan) with End_of_file -> None in
  let rec aux i acc = 
    match try_read () with
    | Some a -> aux (i+1) ((i,a) :: acc)
    | None   -> close_in chan;
    List.rev acc in aux 0 []

(* count number of blancs (offset) in line (string), if it is odd - exception *)
let count_offset st : int =                       (* may be dont need this function *)
  let rec aux st i acc =
    match st.[i] with
    | ' ' -> aux st (i+1) (acc + 1)
    | _   -> if (acc mod 2 = 0) then acc
    else failwith "block alignment failure"        (*TODO may be raise ?*)
  in aux st 0 0

let parse_line (line : int * string) : int * string list =
  let (x, y) = line in  
  let pos = count_offset y in
  let splited = String.split_on_char ' ' y in 
  (pos, splited)

let compose_program (l : (int * string) list) : program =  
  failwith "TODO"

(* TMP *)
let get_string (l : (int * string) list) i : string =  
  let el = List.nth l i in
  let (x,y) = el in y
(***************************  Prints  ***********************************)

(* print simple list *)
let print_list l =
  let rec print_elements = function
    | [] -> ()
    | h::tail -> print_string h;
    print_string ";";
    print_elements tail
  in
  print_string "[";
  print_elements l;
  print_string "]"
  (*print_string "\n"*)

(* TMP *)
let print_list_1 (l : int * string list) : unit =
  let (x,y) = l in
  print_string "[";
  print_int x;
  print_string ":";
  print_list y;
  print_string "]";
  print_string "\n"

(* print list of tuples *)
let print_list_tuples (t : (int * string) list) : unit =
  let rec print_elements = function
    | [] -> ()
    | h::tail -> let (x,y) = h in
      print_int x; print_string ":"; print_string y;
    print_string "; ";
    print_string "\n";
    print_elements tail
  in
  print_string "[";
  print_elements t;
  print_string "]";
  print_string "\n"

(************************  Main functions  ******************************)

let read_polish (filename:string) : program = 
  compose_program(read_file (filename))

let print_polish (p:program) : unit = failwith "TODO"

let eval_polish (p:program) : unit = failwith "TODO"

let usage () =
  print_list_tuples (read_file "/home/nata/Documents/L3_PF/pf5-projet/exemples/fact.p");  
  print_list_tuples (read_file "/home/nata/Documents/L3_PF/pf5-projet/exemples/factors.p");  
  print_string "\n";
  print_string("  WHILE * i i <= n");
  print_string "\n";
  print_list_1(parce_line (2, "  WHILE * i i <= n"));
  print_string "\n"
  (*print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"*)

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> print_polish (read_polish file)
  | [|_;"--eval";file|] -> eval_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
