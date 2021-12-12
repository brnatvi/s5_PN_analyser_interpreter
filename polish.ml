
(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(**********************************************************************************************************************)
(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
type position = int

(** Nom de variable *)
type name = string

(** Type for sliced and splited code *)
type code_lines_t = (int * (string list)) list

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
let empty_string : int = -1

(***********************************************  Exceptions  *********************************************************)

exception ErrorCountOffsets of int
exception FoundOpName of op
exception WrongResult of string
exception WrongSyntax of string

(******************************************* Evaluate functions *******************************************************)
let eval_bop (o:op) =
  match o with
  | Add -> (fun x y -> x + y)
  | Sub -> (fun x y -> x - y)
  | Mul -> (fun x y -> x * y)
  | Div -> (fun x y -> x / y)
  | Mod -> (fun x y -> x mod y)

let rec evaluate_expression (ex:expr) (env: int NameTable.t) =
  match ex with
  | Num x -> x
  | Op (o, x1, x2) ->  eval_bop o (evaluate_expression x1 env) (evaluate_expression x2 env)
  | Var v -> 
    try (NameTable.find v env) with Not_found -> 
      raise (WrongResult (Printf.sprintf "Unknown variable %s" v))
(******************************************  Syntax check up  *********************************************************)
let get_op (st : string) : op =
  match st with
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | "%" -> Mod
  | _   -> raise (WrongResult ("'"^st^"'"^" is not an operation"))

  let get_comp (st : string) : comp =
  match st with
  | "="  -> Eq
  | "<>" -> Ne
  | "<"  -> Lt
  | "<=" -> Le
  | ">"  -> Gt
  | ">=" -> Ge
  |  _   -> raise (WrongResult ("'"^st^"'"^" is not a comparaison")) 

let get_name (e : expr) : name =
  match e with 
  | Var n -> n
  | _ -> raise (WrongResult (" not a variable"))

let get_name_or_var ( a : string) : expr =
    match (int_of_string_opt a) with
    | Some x -> Num x
    | None -> 
      match a with
      | "+" -> raise (FoundOpName Add)
      | "-" -> raise (FoundOpName Sub)
      | "*" -> raise (FoundOpName Mul)
      | "/" -> raise (FoundOpName Div)
      | "%" -> raise (FoundOpName Mod)
      | var -> Var var

(* remove first c elements from list and return obtained list *)
let truncateHead (l : 'a list) (c : int) : 'a list =
  List.filteri (fun x _ -> x >= c) l

(* remove element el from list and return obtained list *)
let removeEl (l : string list) (el : string) : 'a list =
    List.filter (fun x -> x <> el) l

(* analyse syntaxe to compose expression *)
let analize_expr (l : string list) : expr =                 (*TODO  List.length stack calculate before*)
  let rec aux list stack =  
    match list with
    | [] -> if List.length stack = 1 then List.nth stack 0
            else let err_msg = Printf.sprintf "Expression has more than expected tokens: %d" (List.length stack) in 
            raise (WrongResult err_msg)
    | h::tail ->
      try let s = (get_name_or_var h) in aux tail (s::stack)
      with FoundOpName opName ->
        try aux tail (Op(opName, (List.nth stack 0), (List.nth stack 1))::(truncateHead stack 2)) with
        Failure _ -> raise (WrongResult "Expression has less than expected tokens")
  in aux (removeEl (List.rev l) ("")) []  

(* analyse syntaxe to compose condition *)
let analize_cond (l : string list) : cond =
  let rec aux l acc =
  match l with
  | [] -> raise (WrongResult (" wrong comparaison"))
  | h::tail -> 
    match h with
    | "=" | "<>" | "<" | "<=" | ">" | ">=" -> (analize_expr (List.rev acc), get_comp h, analize_expr tail) 
    | a -> aux tail (a::acc)
  in aux l []

(* analyse syntaxe to compose Set *)
let analyse_set (l : string list) : instr=
  let rec aux l acc =
    match l with
    | [] -> raise (WrongResult (" wrong Set"))
    | h::tail -> 
      match h with
      |":=" -> Set (get_name(analize_expr (List.rev acc)), analize_expr tail)
      | a -> aux tail (a::acc)
  in aux l []

(******************************************* Read functions ******************************************************)

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

(* count block offset, each offset = 2 spaces, if it is odd - exception *)
  let rec block_offset (l : string list) (offset : int) : int =    
    match l with
    | [] -> empty_string
    | h::tail -> 
      match h with
      | "" -> block_offset tail (offset + 1)
      | "\n" ->  empty_string
      | _ ->  
        if (offset mod 2 = 0) then offset / 2 
             else raise (ErrorCountOffsets offset) 

(******************************************* Split functions***********************************************************)
let split_line (line : int * string) : string list =
  let (x, y) = line in    
  let splited = String.split_on_char ' ' y in splited

let split_all_code (code : (int * string) list) : code_lines_t =
  let rec aux acc code =
    match code with
    | [] -> acc
    | h::tail -> let (x, y) = h in
     aux ((x, split_line h)::acc) tail
  in aux [] (List.rev code)

(******************************************* Build block***************************************************************)

(* append element to list and return obtained list*)
let rec append l i =
  match l with
  | [] -> [i]
  | h::t -> h::(append t i)

(* check the syntax of potential ELSE block, 
return true if it is correct and the block ELSE present, and false if it is not present (IF without ELSE) *)
let rec is_else (_p, line) (exp_offset: int) (offset:int) (p : int) : bool =
  match line with
  | [] -> false
  | x::xs -> 
    match x with
      | "" -> is_else (_p, xs) (exp_offset) (offset+1) (p)
      | "ELSE" -> if ((exp_offset*2)=offset) then true 
                  else let err_msg = Printf.sprintf "Wrong ELSE alignment at line  %d" p in 
                  raise (WrongResult err_msg)
      | _ -> false      

(* compose block of programe *)
let build_block (txt : code_lines_t) : block =   
  let rec scan_lines (lines : code_lines_t) (cur_block : block) (offset : int) : (code_lines_t * block) =  
    match lines with  
    | [] -> ([], cur_block)
    | h::tail -> 
      let (p, line) = h in 
      let cur_offset = block_offset line 0 in
      if (cur_offset = offset) then (* line belongs to current block*)
        let rec scan_instruction (l : string list) (c_code : code_lines_t) (c_bl: block) (c_off : int) (p:int) : (code_lines_t * block) =
          match l with
          | [] -> (tail, c_bl)  (* end of line *) 
          | x::xs -> 
            match x with
            | ""        -> scan_instruction (xs) (c_code) (c_bl) (c_off) (p)
            | "COMMENT" -> scan_lines (tail) (cur_block) (offset)
            | "WHILE"   -> let (rest, sub_block) = scan_lines (tail) [] (c_off + 1) in
                            let wh = (p, (While (analize_cond xs, sub_block))) in
                            scan_lines (rest) (append c_bl wh) (c_off) (*continue scan the rest*)
            | "IF"      -> let (rest, sub_bl1) = scan_lines (tail) [] (c_off + 1) in
                              let else_line = try (List.hd rest) with Failure _ -> (0, []) in
                              if is_else (else_line) (c_off) (0) (p) then
                                let (else_tail, sub_bl2) = scan_lines (List.tl rest) [] (c_off + 1) in
                                let if_else = (p, (If ((analize_cond xs), sub_bl1, sub_bl2))) in
                                  scan_lines (else_tail) (append c_bl if_else) (c_off) (*continue scan the rest*)
                              else   
                                let iff = (If ((analize_cond xs), sub_bl1, [])) in 
                                scan_lines (rest) (append c_bl (p, iff)) (c_off) (*continue scan the rest*)
            | "PRINT"   -> (tail, append c_bl (p, (Print (analize_expr xs))))  
            | "READ"    -> (tail, append c_bl (p, (Read (get_name(analize_expr xs)))))  
            | _         -> (tail, append c_bl (p, (analyse_set line)))
        in let (rest, cur_block) = scan_instruction (line) (lines) (cur_block) (offset) (p) 
        in scan_lines (rest) (cur_block) (offset)
      else if (cur_offset < offset) then (* line belongs to one of the parent blocks*)
        (lines, cur_block) 
      else if (cur_offset = empty_string) then (* if empty_line ...*)
        scan_lines (tail) (cur_block) (offset)      
      else  (*exception - wrong alignment*)
        let err_msg = Printf.sprintf "Wrong block alignment at line   %d" p in 
        raise (WrongResult err_msg)
  in match scan_lines txt [] 0 with
    | (_, root_block) -> root_block
 
(******************************************* Print functions **********************************************************)
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
  | Op (op, e1, e2) -> print_op op; print_string " "; print_expr e1; print_string " "; print_expr e2

let print_cond (c: cond) : unit =    
  let (ex1, compar, _ex2) = c in
  print_expr ex1; print_string " "; print_comp compar; print_string " "; print_expr _ex2
 
let rec print_align (depth:int) : unit =
  match depth with
  | 0 -> ()
  | _ -> print_string "  "; print_align (depth - 1)
    
let rec print_block (b: block) (d : int) : unit =    
    match b with
    | [] -> () 
    | h::tail -> let (_, l) = h in   
    match l with
    | Set (n, ex)      -> print_align d; 
                          print_string n; print_string " := "; print_expr ex; print_string "\n"; 
                          print_block tail d
    | Read n           -> print_align d; 
                          print_string "READ "; print_string n; print_string "\n"; 
                          print_block tail d
    | Print ex         -> print_align d; 
                          print_string "PRINT "; print_expr ex; print_string "\n"; 
                          print_block tail d
    | If (c, bl1, bl2) -> print_align d; 
                          print_string "IF "; print_cond c; print_string "\n";
                          print_block bl1 (d + 1);       
                          if (List.length bl2) > 0 then print_align d; 
                          if (List.length bl2) > 0 then print_string "ELSE\n";
                          print_block bl2 (d + 1); print_block tail d
    | While (c, bl)    -> print_align d; 
                          print_string "WHILE "; print_cond c; print_string "\n";
                          print_block bl (d + 1); 
                          print_block tail d

(******************************************* Main functions ***********************************************************)
let read_polish (filename:string) : program = 
  build_block(split_all_code(read_file(filename)))

let print_polish (p:program) : unit = 
  print_block (p) 0

let eval_polish (p:program) : unit = failwith "TODO"

let usage () =
  print_string "Polish : mini-language static analizer\n\n";
  print_string "Usage: dune exec -- ./polish.exe [option] filename \n\n";
  print_string "Options: \n";
  print_string "--reprint - analyze input file and reprint program\n";
  print_string "--eval    - execute input file\n"

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> print_polish (read_polish file)
  | [|_;"--eval";file|] -> eval_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
