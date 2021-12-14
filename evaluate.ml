open Types

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

let evaluate_condition (cm:comp) (ex1:expr) (ex2:expr) (env: int NameTable.t) : bool =
  match cm with
  | Eq -> (evaluate_expression ex1 env) =  (evaluate_expression ex2 env)(* = *)
  | Ne -> (evaluate_expression ex1 env) <> (evaluate_expression ex2 env)(* Not equal, <> *)
  | Lt -> (evaluate_expression ex1 env) <  (evaluate_expression ex2 env)(* Less than, < *)
  | Le -> (evaluate_expression ex1 env) <= (evaluate_expression ex2 env)(* Less or equal, <= *)
  | Gt -> (evaluate_expression ex1 env) >  (evaluate_expression ex2 env)(* Greater than, > *)
  | Ge -> (evaluate_expression ex1 env) >= (evaluate_expression ex2 env)(* Greater or equal, >= *)

let rec evaluate_block (bl:block) (env: int NameTable.t) : int NameTable.t =
  match bl with  
  | [] -> env
  | h::tail -> 
    let (_, inst) = h in 
      match inst with
      | Set (nm, ex) -> let env = NameTable.add nm (evaluate_expression ex env) env in (evaluate_block tail env)
      | Read (nm) -> Printf.printf "Enter %s variable value: " nm;
                      let env = NameTable.add nm (read_int()) env in (evaluate_block tail env)
      | Print (ex) -> Printf.printf "%d\n" (evaluate_expression ex env); (evaluate_block tail env)
      | If (cd, bl1, bl2) -> let (ex1, cd, ex2) = cd in
                               if (evaluate_condition (cd) (ex1) (ex2) (env))
                               then (evaluate_block tail (evaluate_block bl1 env))
                               else (evaluate_block tail (evaluate_block bl2 env))
      | While (cd, wbl) -> let (ex1, cd, ex2) = cd in
                            if (evaluate_condition (cd) (ex1) (ex2) (env))
                            then (evaluate_block bl (evaluate_block wbl env))
                            else (evaluate_block tail env)