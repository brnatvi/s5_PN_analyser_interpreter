open Types
open Prints

(************************************************ Vars ****************************************************************)

(* check if expresiion is present in set *)
let rec is_present (e:expr) (vars: Names.t) : bool =  
  match e with 
  | Num a -> true
  | Var n -> Names.mem n vars
  | Op (op, ex1, ex2) -> (is_present ex1 vars) && (is_present ex2 vars)

(* check if condition is present in set *)
let is_present_cond (c:cond) (vars: Names.t) : bool =  
  let (ex1, comp, ex2) = c in
  (is_present ex1 vars) && (is_present ex2 vars)

(* if expression is not in 'v_initialized' set add it to 'v_initialized' and 'v_all'*)  
let rec add_everywhere (e:expr) (v_all:Names.t) (v_initialized:Names.t) : (Names.t * Names.t) = 
  match e with 
  | Num a -> (v_all, v_initialized)
  | Var n ->  if (Names.mem n v_initialized) = false then
                (Names.add n v_all, Names.add n v_initialized)
              else (v_all, v_initialized)
  | Op (op, ex1, ex2) -> let (x, y) = (add_everywhere ex1 v_all v_initialized) in
                          add_everywhere ex2 x y

(* add expression to set *)                          
let rec add_expr (e:expr) (s:Names.t) : Names.t = 
  match e with 
  | Num a -> s
  | Var n -> Names.add n s
  | Op (op, ex1, ex2) -> let x = add_expr ex1 s in
                          add_expr ex2 x

(* add condition to set *) 
let add_cond (c:cond) (s:Names.t) : Names.t = 
  let (ex1, comp, ex2) = c in
  match (is_present ex1 s) with
  | true -> if (is_present ex2 s) then s
            else add_expr ex2 s
  | false -> if (is_present ex2 s) then add_expr ex1 s
              else add_expr ex2 (add_expr ex1 s)                           

(* compose two sets - first one with all variables, second one with initialised variables*)              
let rec vars_program (bl:block) (v_all:Names.t) (v_initialized:Names.t) : (Names.t * Names.t) =  
  match bl with  
  | [] -> (v_all, v_initialized)
  | h::tail -> 
    let (_, inst) = h in 
      match inst with
      | Set (nm, ex) -> if is_present ex v_initialized then   
                          vars_program tail (Names.add nm v_all) (Names.add nm v_initialized)    
                        else vars_program tail (Names.add nm v_all) v_initialized                 

      | Read (nm) ->  let (x,y) = (add_everywhere (Var nm) v_all v_initialized) in                       
                      vars_program tail x y

      | Print (ex) -> if (is_present ex v_initialized) then 
                        let (x,y) = (add_everywhere ex v_all v_initialized) in                       
                        vars_program tail x y
                      else vars_program tail (add_expr ex v_all) v_initialized

      | While (cd, wbl) -> if (is_present_cond cd v_initialized) then 
                              let (x, y) = (vars_program wbl v_all v_initialized) in                      
                              vars_program tail x v_initialized  

                            else                               
                              let (k, m) = (vars_program wbl (add_cond cd v_all) v_initialized) in                   
                              vars_program tail k v_initialized
       
      | If (cd, bl1, bl2) -> if (is_present_cond cd v_initialized) then 
                                let (x, y) = (vars_program bl1 v_all v_initialized) in
                                let varsBl1 = Names.diff y v_initialized in

                                let (a, b) = (vars_program bl2 x (Names.diff y varsBl1)) in
                                let varsBl2 = Names.diff b v_initialized in
                                
                                (*let () = print_string "first block:\n" in
                                let () = print_set (varsBl1) in 
                                let () = print_string "\nsecond block:\n" in 
                                let () = print_set (varsBl2) in
                                let () = print_string "\n---------------\n" in *)
                                let varsBlIf = Names.inter varsBl1 varsBl2 in
                               vars_program tail a (Names.union v_initialized varsBlIf)                          
                                         
                              else                                          
                                let (x, y) = (vars_program bl1 (add_cond cd v_all) v_initialized) in
                                let varsBl1 = Names.diff y v_initialized in

                                let (a, b) = (vars_program bl2 x (Names.diff y varsBl1)) in
                                let varsBl2 = Names.diff b v_initialized in
                                
                                let varsBlIf = Names.inter varsBl1 varsBl2 in
                                vars_program tail a (Names.union v_initialized varsBlIf)
     