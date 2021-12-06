
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
let get_op (st : string) : op =
  match st with
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | "%" -> Mod
  | _   -> failwith "not an operation"

let get_comp (st : string) : comp =
  match st with
  | "="  -> Eq
  | "<>" -> Ne
  | "<"  -> Lt
  | "<=" -> Le
  | ">"  -> Gt
  | ">=" -> Ge
  |  _   -> failwith "not a comparaison"
  
let analize_variable (st : string) : name =
    if ((st.[0] = '+') || (st.[0] = '-') || (st.[0] = '*') || (st.[0] = '/') || (st.[0] = '%')) then failwith "not a variable"
    else st 

let check_assignment (st : string) : bool =  
  (st.[0] = ':') && (st.[1] = '=')

let get_name_or_var a =
    match (int_of_string_opt a) with
    | Some x -> Num x
    | None -> 
      match a with
      | "+" -> Op (Add, Num 0, Num 0)
      | "-" -> Op (Sub, Num 0, Num 0)
      | "*" -> Op (Mul, Num 0, Num 0)
      | "/" -> Op (Div, Num 0, Num 0)
      | "%" -> Op (Mod, Num 0, Num 0)
      | var -> Var var

let truncateHead (lst : 'a list) (count : int) : 'a list =
  List.filteri (fun x _ -> x >= count) lst

let analize_expr (l : string list) : expr =   
  let lRev = List.rev l in
  let rec aux list stack =  
    match list with
    | [] -> List.nth stack 0
    | _ ->    
    match (get_name_or_var (List.hd list)) with
    | Num k -> aux (List.tl list) ((Num k)::stack)
    | Var k -> aux (List.tl list) ((Var k)::stack)
    | Op (opName, _, _) -> aux (List.tl list) (Op (opName, (List.nth stack 0), (List.nth stack 1))::(truncateHead stack 2))   
  in aux lRev [] 
  

let analize_cond (l : string list) : cond =
  let l1 = [] in
  let rec aux l acc =
  match l with
  | [] -> failwith "wrong condition"
  | h::tail -> 
    match h with
    | "=" | "<>" | "<" | "<=" | ">" | ">=" -> (analize_expr (List.rev acc), get_comp h, analize_expr tail) 
    | a -> aux tail (a::acc)
  in aux l []


(************************  Exceptions  ******************************)

exception ErrorCountOffsets of int

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
    List.rev acc in aux 1 []

(* count number of blancs (offset) in line (string), if it is odd - exception *)
let count_offset st : int =                       (* may be dont need this function *)
  let rec aux st i acc =
    match st.[i] with
    | ' ' -> aux st (i+1) (acc + 1)
    | _   -> if (acc mod 2 = 0) then acc
    else raise (ErrorCountOffsets acc)       (*TODO may be raise ?*)
  in aux st 0 0

let splite_line (line : int * string) : string list =
  let (x, y) = line in    
  let splited = String.split_on_char ' ' y in splited

let split_all_code (code : (int * string) list) : (int * (string list)) list =
  let rec aux acc code =
    match code with
    | [] -> acc
    | h::tail -> let (x, y) = h in
     aux ((x, splite_line h)::acc) tail
  in aux [] (List.rev code)

let compose_program (l : (int * (string list)) list) : program =   
(*let rec aux acc p list =       (* inspect lines of code p : position; acc : accumulator for program*)
  match list with  
  | [] -> acc
  | h::tail -> let (x, y) = h in    (* inspect words of line x : int , y : string list *)
  let rec aux2 i offset =
    match y with
    | [] -> aux acc (p+1) list      (* if line is numerated but empty.. what if it is the last one ?*)
    | a::tail2 -> match a with
      | "" -> aux2 i (offset+1)               (* it's an offset *)          
      | "READ" -> let e = get_name(analize_expr tail2) in            (* need to check if is number after READ *)
     let name = Read e in
     let acc = (p,i)::acc in aux acc (p+1)   
      | "PRINT" -> (*let e = analize_expr tail2 in            (* need to check if is number after READ *)
        let e = Print e in
        let acc = (p,i)::acc in aux acc (p+1) *)failwith "TODO"              (* need to check if is instr after PRINT *)
      | "IF" -> failwith "TODO"                 (* need to check if is cond*instr*instr after IF and create the blocks *)
      | "WHILE" -> failwith "TODO"
      | "COMMENT" -> failwith "TODO"            (* need ? *)
      | _ -> failwith "TODO"       ":="  (*filling of TabName*)        (* it's a variable *)
    in aux2 0 0
  in aux [] 1 l*)failwith "TODO"


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

let print_tuple_int_st (t : (int * string list)) : unit =
  let (x,y) = t in
  print_string "(";
  print_int x;
  print_string ",";
  print_list y;
  print_string ")";
  print_string "\n"

let print_list_of_tuples (l : (int * (string list)) list) : unit =   
  print_string "[";
  List.iter (fun (h) -> let (x,y) = h in print_tuple_int_st h) l;
  print_string "]";
  print_string "\n"
  

(* TMP *)
let print_list_tuples_1 (t : (int * string) list) : unit =
  let rec print_elements = function
    | [] -> ()
    | h::tail -> let (x,y) = h in
    print_string "(";
    print_int x; print_string ","; print_string y;
    print_string ");";
    print_string "\n";
    print_elements tail
  in
  print_string "[";
  print_elements t;
  print_string "]";
  print_string "\n"

let print_op (o: op) : unit =
  match o with
  | Add -> print_string "+"  
  | Sub -> print_string "-"  
  | Mul -> print_string "*"  
  | Div -> print_string "/"  
  | Mod -> print_string "%"  

let print_comp (c: comp) : unit =
  match c with  
  | Eq -> print_string "= " 
  | Ne -> print_string "<>"
  | Lt -> print_string "<" 
  | Le -> print_string "<="
  | Gt -> print_string ">" 
  | Ge -> print_string ">="

let rec print_expr (e: expr) : unit =
  match e with
  | Num x -> print_int x
  | Var v -> print_string v
  | Op (op, e1, e2) -> print_string "("; print_op op; print_string ","; print_expr e1; print_string ","; print_expr e2; print_string ")"


let print_cond (c: cond) : unit =
  let (e1, comp, e2) = c in
  print_string "(";
  print_expr e1;
  print_comp comp;
  print_expr e2;  
  print_string ")"
  (* print_string "("; print_op op; print_string ","; print_expr e1; print_string ","; print_expr e2; print_string ")"*)
(************************  Evaluation  ******************************)

(************************  Main functions  ******************************)

let read_polish (filename:string) : program = 
  compose_program(split_all_code(read_file(filename)))

let print_polish (p:program) : unit = failwith "TODO"

let eval_polish (p:program) : unit = failwith "TODO"

let usage () =
  (*print_list_tuples (read_file "/home/nata/Documents/L3_PF/pf5-projet/exemples/fact.p");  
  print_list_tuples (read_file "/home/nata/Documents/L3_PF/pf5-projet/exemples/factors.p");  
  print_string "\n"; *)
  print_expr(analize_expr (["+";"1";"*";"3";"n";]));
  print_string "\n";
  print_expr(analize_expr (["1"]));
  print_string "\n";
  print_expr(analize_expr (["name"]));
  print_string "\n";
  print_expr(analize_expr (["+";"-";"20";"*";"3";"4";"1";]));
  print_string "\n";
  print_cond(analize_cond(["n";"<";"-";"4";"1";]));
  print_string "\n";
  print_cond(analize_cond(["-";"n";"20";"<";"-";"4";"1";]));
  print_string "\n";

  (*print_expr(analize_expr (["+";"-";"20";"*";"3";"4";]));           (* wrong expression*)
  print_string "\n";
  print_expr(analize_expr (["+";"-";"20";"*";"3";"4";"-";]));           (* wrong expression*)
  print_string "\n";*)
  print_expr(analize_expr (["%"]));
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
