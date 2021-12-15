open Types

val evaluate_block : block -> int NameTable.t -> int NameTable.t
(** evaluates a block of instructions *)