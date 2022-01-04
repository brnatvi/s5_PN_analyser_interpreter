open Evaluate
open Types
open Prints
open Syntax

(*********************************************** Exceptions *********************************************************)
exception ByZero of string

(******************************************* Simplify functions **********************************************************)

let simpl_add (e1, e2) =
match (e1, e2) with
| Num x, Num y -> Num (x + y)
| e1, e2 -> if e1 = Num 0 then e2
else if e2 = Num 0 then e1 else Op(Add, e1, e2)

let simpl_sub (e1, e2) =
match (e1, e2) with
| Num x, Num y -> Num (x - y)
| e1, e2 -> if e2 = Num 0 then e1
else Op(Sub, e1, e2)

let simpl_mul (e1, e2) =
match (e1, e2) with
| Num x, Num y -> Num (x * y)
| e1, e2 -> if e1 = Num 0 || e2 = Num 0 then Num 0
else if e1 = Num 1 then e2 
else if e2 =  Num 1 then e1
else Op(Mul, e1, e2)

let simpl_div (e1, e2) =
match (e1, e2) with
| Num x, Num y -> if y = 0 then raise (ByZero ("division by zero"))
else Num (x / y)
| e1, e2 -> if e2 = Num 0 then raise (ByZero ("division by zero"))
else if e1 = Num 0 then Num 0
else if e2 = Num 1 then e1
else Op(Div, e1, e2)

let simpl_mod (e1, e2) =
match (e1, e2) with
| Num x, Num y -> Num (x mod y)
| e1, e2 -> if e2 = Num 0 then raise (ByZero ("division by zero"))
else if e1 = Num 0 then Num 0
else if e2 = Num 1 then Num 0
else Op(Mod, e1, e2)

let simpl_bop op e1 e2 =
match op with
| Add -> simpl_add (e1, e2)
| Sub -> simpl_sub (e1, e2)
| Mul -> simpl_mul (e1, e2)
| Div -> simpl_div (e1, e2)
| Mod -> simpl_mod (e1, e2)

let rec simpl_expr e =
match e with 
| Num x -> Num (x)
| Var x -> Var(x)
| Op (op, x, e') -> simpl_bop op (simpl_expr x) (simpl_expr e')

(* let rec simpl_progr (b: block) : block =
match b with 
| [] -> []
| h::tail -> let (_, l) = h in 
match l with
| Set (n, ex) -> print_string "ww\n"; print_expr (simpl_expr ex) ; print_string "\n"; simpl_progr tail
| Read n -> print_string "RR\n"; simpl_progr tail
| _ -> print_string "ok\n"; simpl_progr tail *)

(* let rec simpl_progr (b: block) : block=
  match b with 
  | [] -> []
  | h::tail -> let (_, l) = h in 
  match l with
  | Set (n, ex) -> (simpl_expr ex) in (simpl_progr tail)
  | Read n -> simpl_progr tail
  | _ -> simpl_progr tail *)

(* let see (b: block) : block =
  let rec aux cur acc =
  match cur with 
  | [] -> acc
  | h::tail -> 
  match h with
  | Set (n, ex) -> aux tail (acc)
  | _ -> aux tail acc
  in aux b [] *)
  (* | Read n -> simpl_progr tail *)


  let simpl_instr (i: instr) : instr =
    match i with 
    | Set (n, ex) -> Set (n, simpl_expr ex)
    | _ -> i
(* (parc, set - prnt - if - while.... ) *)

let see (b: block) : block =
  let rec aux bl acc =
  match b with 
  | [] -> acc
  | h::tail -> let (vv, vv') = h in aux tail ((vv, simpl_instr vv')::acc)
  in aux b []

let rec simpl_expr(e: expr) : expr = 
  match e with
  | Num x -> Num x
  | Var x -> Var x
  | Op (o, e1, e2) ->
    match (o, simpl_expr (e1), simpl_expr (e2)) with
    | (Mul, Num 0, _) -> Num 0    (* x*0 = 0 *)
    | (Mul, _, Num 0) -> Num 0    (* 0*x = 0 *)
    | (Add, ex1, Num 0) -> ex1    (* 0+x = x *)
    | (Add, Num 0, ex2) -> ex2    (* x+0 = x *)
    | (Sub, ex1, Num 0) -> ex1    (* x-0 = x *)
    | (Mul, ex1, Num 1) -> ex1    (* x*1 = x *)
    | (Mul, Num 1, ex2) -> ex2    (* 1*x = x *)
    | (Div, ex1, Num 1) -> ex1    (* x/1 = x *)
    | (Div, Num 0, ex2) -> Num 0  (* 0/x = 0 *)
    | (Mod, ex1, Num 1) -> Num 0  (* x%1 = 0 *)
    | (o, Num n1, Num n2) -> Num (eval_bop o n1 n2)
    | (o, ex1, ex2) -> Op(o, ex1, ex2) (* return initial expression *)

let simpl_condition (cm:comp) (a1:int) (a2:int) : bool =
  match cm with
  | Eq -> a1 =  a2 (* = *)
  | Ne -> a1 <> a2 (* Not equal, <> *)
  | Lt -> a1 <  a2 (* Less than, < *)
  | Le -> a1 <= a2 (* Less or equal, <= *)
  | Gt -> a1 >  a2 (* Greater than, > *)
  | Ge -> a1 >= a2 (* Greater or equal, >= *)