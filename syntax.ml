open Types
(***********************************************  Exceptions  *********************************************************)

exception ErrorCountOffsets of int
exception FoundOpName of op
exception WrongResult of string
exception WrongSyntax of string

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

(******************************************* Build block***************************************************************)
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

