open Evaluate
open Types
open Prints
open Syntax
open Vars
open Simpl

let sign_compare(s1: sign)(s2: sign) : int =
  if s1 == s2 then 0
  else if s1 > s2 then 1
  else -1 

(*prints*)
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

(* comparation *)
let is_signes_equal(s1 : sign list)(s2 : sign list) : bool =
  let rec aux l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | [], _ -> false
    | _, [] -> false
    | c::cc, d::dd -> if c == d then aux cc dd else false
  in aux (List.sort sign_compare s1) (List.sort sign_compare s2)

let is_vars_signes_equal(m1 : sign list NameTable.t) (m2 : sign list NameTable.t): bool =
  NameTable.equal is_signes_equal m1 m2

(* add *)  
let rec add_signes(sl1 : sign list) (sl2 : sign list): sign list =
  let add_one_sign s sl = 
    try (let _ = List.find (fun a -> a = s) sl in sl ) with Not_found -> s::sl in
      match sl1 with
      | [] -> sl2
      | h::t -> let sl2 = add_one_sign h sl2 in add_signes t sl2



(* analysis of cases*)  
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

let get_bop_signes(o:op)(s1:sign)(s2:sign): sign list =
  match o with
  | Add -> get_plus_signes s1 s2
  | Sub -> get_minus_signes s1 s2
  | Mul -> get_mul_signes s1 s2
  | Div
  | Mod -> get_div_signes s1 s2

