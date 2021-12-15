
(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

open Evaluate
open Types
open Prints
open Syntax

(***********************************************  Exceptions  *********************************************************)

exception ErrorCountOffsets of int
exception FoundOpName of op
exception WrongResult of string
exception WrongSyntax of string

(******************************************* Read and split functions ******************************************************)

(* read code from file and store it to list *)
let read_file (filename:string) : ((int * string) list) =  
  let chan = Stdlib.open_in filename in
  let try_read () =
    try Some (input_line chan) with End_of_file -> None in
  let rec aux i acc = 
    match try_read () with
    | Some a -> aux (i+1) ((i,a) :: acc)
    | None   -> close_in chan;
    List.rev acc in aux 1 []

let split_line (line : int * string) : string list =
  let (x, y) = line in    
  let splited = String.split_on_char ' ' y in splited

let split_all_code (code : (int * string) list) : code_lines_t =
  let rec aux acc code =
    match code with
    | [] -> acc
    | h::tail -> let (x, y) = h in
     aux ((x, split_line h)::acc) tail
  in aux [] (List.rev code)
 
(******************************************* Main functions ***********************************************************)
let read_polish (filename:string) : program = 
  build_block(split_all_code(read_file(filename)))

let print_polish (p:program) : unit = 
  print_block p 0

let eval_polish (p:program) : unit = 
  let _ = evaluate_block p NameTable.empty in ()

let usage () =
  print_string "Polish : mini-language static analizer\n\n";
  print_string "Usage: dune exec -- ./polish.exe [option] pathfile \n\n";
  print_string "   or: ./run [option] pathfile \n\n";
  print_string "Options: \n";
  print_string "--reprint - analyze input file and reprint program\n";
  print_string "--eval    - execute input file\n"

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> print_polish (read_polish file)
  | [|_;"--eval";file|] -> eval_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
