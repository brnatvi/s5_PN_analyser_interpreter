open Evaluate
open Types
open Syntax

(* simplify expressions with 0 and 1 *)
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
    | (o, Num n1, Num n2) -> Num (eval_bop o n1 n2) (* calculate *)
    | (o, ex1, ex2) -> Op(o, ex1, ex2) (* return initial expression *)

(* evaluate condition with constants *)
let eval_const_condition (cm:comp) (a1:int) (a2:int) : bool =
  match cm with
  | Eq -> a1 =  a2 (* = *)
  | Ne -> a1 <> a2 (* Not equal, <> *)
  | Lt -> a1 <  a2 (* Less than, < *)
  | Le -> a1 <= a2 (* Less or equal, <= *)
  | Gt -> a1 >  a2 (* Greater than, > *)
  | Ge -> a1 >= a2 (* Greater or equal, >= *)

  (* apply the above functions to concrete expressions *)
let rec simpl_block (oldB: block) (newB: block) (pos : int) : block * int =
  match oldB with  
  | [] -> (newB, pos)
  | h::tail -> 
    let (_, inst) = h in 
      match inst with
      | Set (nm, ex) -> let newB = append newB (pos, Set (nm, simpl_expr ex)) in simpl_block tail newB (pos+1)
      | Read (nm) -> let newB = append newB (pos, Read (nm)) in simpl_block tail newB (pos+1)
      | Print (ex) -> let newB = append newB (pos, Print (simpl_expr ex)) in simpl_block tail newB (pos+1)
      | If (cd, bl1, bl2) -> 
        let (ex1, comp, ex2) = cd in 
        let (pex1, pex2) = (simpl_expr ex1, simpl_expr ex2) in 
        (match (pex1, pex2) with 
         | (Num n1, Num n2) -> 
           if eval_const_condition comp n1 n2 then
             let (ifB, pos) = simpl_block bl1 [] (pos+1) in  
               let newB = List.append newB ifB in simpl_block tail newB (pos+1)
           else             
            (match bl2 with
            | [] -> simpl_block tail newB (pos+1)  (*if else block is empty - skip entire if*)
            | _ -> let (ifB, pos) = simpl_block bl2 [] (pos+1) in  
                  let newB = List.append newB ifB in simpl_block tail newB (pos+1)
            )
         | (_, _) ->  
          let (ifB1, pos) = simpl_block bl1 [] (pos+1) in  
          let (ifB2, pos) = simpl_block bl2 [] (pos+1) in  
            let newB = append newB (pos, If ((pex1, comp, pex2), ifB1, ifB2)) in simpl_block tail newB (pos+1)   
        )
      | While (cd, wbl) -> 
        let (ex1, comp, ex2) = cd in 
        let (pex1, pex2) = (simpl_expr ex1, simpl_expr ex2) in  (*OK*)
          (match (pex1, pex2) with 
           | (Num n1, Num n2) -> 
             if eval_const_condition comp n1 n2 then
               let (ifB, pos) = simpl_block wbl [] (pos+1) in
                 let newB = append newB (pos, While ((pex1, comp, pex2), ifB)) in simpl_block tail newB (pos+1)   
             else
               simpl_block tail newB (pos+1)
           | (_, _) -> 
             let (ifB, pos) = simpl_block wbl [] (pos+1) in
               let newB = append newB (pos, While ((pex1, comp, pex2), ifB)) in simpl_block tail newB (pos+1)   
          )

let simple_polish (p:program) : program = 
 let (p, _) = simpl_block p [] 1 in p