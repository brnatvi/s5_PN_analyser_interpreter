open Types

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
                          
                          
let print_set (s:Names.t) = Names.iter (fun (x) -> print_string x; print_string " ") s