

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
