open Types

val vars_program : program -> Names.t -> Names.t -> (Names.t * Names.t)
(** Composes two sets of variables: 
first one with all variables presents in code, seconde one with variables accessible after their initialisation *)