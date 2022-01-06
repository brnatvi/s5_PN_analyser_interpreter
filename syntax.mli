open Types

val build_block : code_lines_t -> block
(** builds a program block from a choosen segment *)

val append : 'a list -> 'a -> 'a list
(** appends an element *)

val get_name : expr -> name 
(** gets name from expression *)