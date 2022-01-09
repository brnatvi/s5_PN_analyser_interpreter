open Evaluate
open Types
open Prints
open Syntax
open Vars
open Simpl

let sign_compare(s1: sign)(s2: sign) : int =
  if s1 = s2 then 0
  else if s1 > s2 then 1
  else -1

(* prints *)
let rec print_signes (s : sign list) : unit =
  match s with
  | [] -> Printf.printf "\n"
  | h::tl -> 
  (  match h with
    | Neg  -> Printf.printf "-"
    | Zero -> Printf.printf "0"
    | Pos  -> Printf.printf "+"
    | Error-> Printf.printf "!"
  ); print_signes (tl)

let print_vars_signes (m : sign list NameTable.t) : unit =
  NameTable.iter (fun key value -> Printf.printf "%s " key; print_signes (List.sort sign_compare value)) m

(* equality tests *)
let is_signes_equal(s1 : sign list)(s2 : sign list) : bool =
  let rec aux l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | [], _ -> false
    | _, [] -> false
    | c::cc, d::dd -> if c = d then aux cc dd else false
  in aux (List.sort sign_compare s1) (List.sort sign_compare s2)

let is_vars_signes_equal(m1 : sign list NameTable.t) (m2 : sign list NameTable.t): bool =
  NameTable.equal is_signes_equal m1 m2

(* analize of cases*)  
let get_plus_signes(s1:sign)(s2:sign): sign list =
  match s1, s2 with
  | Pos, Pos
  | Pos, Zero
  | Zero, Pos  -> [Pos]
  | Error, _ 
  | _, Error   -> [Error]
  | Zero, Zero -> [Zero]
  | Neg, Pos
  | Pos, Neg   -> [Zero; Pos; Neg]
  | Neg, Neg   -> [Neg]
  | Neg, Zero
  | Zero, Neg  -> [Neg]

let get_minus_signes(s1:sign)(s2:sign): sign list =
  match s1, s2 with
  | Pos, Pos   -> [Zero; Pos; Neg] 
  | Pos, Zero  -> [Pos]
  | Zero, Pos  -> [Neg]
  | Error, _ 
  | _, Error   -> [Error]
  | Zero, Zero -> [Zero]
  | Neg, Pos   -> [Neg]
  | Pos, Neg   -> [Pos]
  | Neg, Neg   -> [Zero; Pos; Neg] 
  | Neg, Zero  -> [Neg]
  | Zero, Neg  -> [Pos]

let get_mul_signes(s1:sign)(s2:sign): sign list =
  match s1, s2 with
  | Pos, Pos   -> [Pos] 
  | Pos, Zero 
  | Zero, Pos  -> [Zero]
  | Error, _ 
  | _, Error   -> [Error]
  | Zero, Zero -> [Zero]
  | Neg, Pos   -> [Neg]
  | Pos, Neg   -> [Neg]
  | Neg, Neg   -> [Pos] 
  | Neg, Zero  
  | Zero, Neg  -> [Zero]

let get_div_signes(s1:sign)(s2:sign): sign list =
  match s1, s2 with
  | Pos, Pos   -> [Pos] 
  | Pos, Zero 
  | Neg, Zero  
  | Zero, Zero
  | Error, _ 
  | _, Error   -> [Error]
  | Zero, Neg 
  | Zero, Pos  -> [Zero]
  | Neg, Pos   -> [Neg]
  | Pos, Neg   -> [Neg]
  | Neg, Neg   -> [Pos] 

let get_bop_signes(o:op)(s1:sign)(s2:sign) : sign list =
  match o with
  | Add -> get_plus_signes s1 s2
  | Sub -> get_minus_signes s1 s2
  | Mul -> get_mul_signes s1 s2
  | Div
  | Mod -> get_div_signes s1 s2

(* sign of expression *)
let rec merge_lists_signes(sl1 : sign list) (sl2 : sign list): sign list =
  let add_one_sign s sl = 
    if List.mem s sl then sl else s::sl in    
      match sl1 with
      | [] -> sl2
      | h::t -> let sl2 = add_one_sign h sl2 in merge_lists_signes t sl2  

let rec get_signes(o : op)(s : sign)(sl : sign list)(res : sign list) : sign list =
  match sl with
  | []      -> res
  | h::tail -> let res = merge_lists_signes (get_bop_signes o s h) res in get_signes o s tail res

let rec deduct_signes_op (o : op)(sl1 : sign list)(sl2: sign list)(res : sign list) : sign list =
    match sl1 with
    | []      -> res
    | h::tail -> let res = merge_lists_signes (get_signes o h sl2 []) res in deduct_signes_op o tail sl2 res
    
let rec get_expression_signes(e : expr)(m : sign list NameTable.t) : sign list = 
  match e with
  | Num x -> if (x = 0) then [Zero] else if x > 0 then [Pos] else [Neg]
  | Var v -> (try (NameTable.find v m) with Not_found -> [])           
  | Op (o, x1, x2) -> deduct_signes_op o (get_expression_signes x1 m) (get_expression_signes x2 m) []



(* principal functions *)

let get_block_signes (bl : block) (m : sign list NameTable.t) (pErr : int) : (int * sign list NameTable.t) =
  failwith "TODO"
  
let sign_polish (p:program) : unit =
  let (pErr, p) = get_block_signes p (NameTable.empty) (-1) in
  let () = print_vars_signes (p) in
    if (pErr > -1) then Printf.printf "divbyzero %d\n" pErr
    else Printf.printf "safe\n"

