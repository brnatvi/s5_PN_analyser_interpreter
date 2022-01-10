open Types

val evaluate_block : block -> int NameTable.t -> int NameTable.t
(* evaluate a block of instructions *)
val eval_bop : op -> int -> int -> int
(* evaluate arithmetic binary operator *)