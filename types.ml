(**********************************************************************************************************************)
(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
type position = int

(** Nom de variable *)
type name = string

(** Type for sliced and splited code *)
type code_lines_t = (int * (string list)) list

(** Opérateurs arithmétiques : + - * / % *)
type op = Add | Sub | Mul | Div | Mod

(** Expressions arithmétiques *)
type expr =
  | Num of int
  | Var of name
  | Op of op * expr * expr

(** Opérateurs de comparaisons *)
type comp =
| Eq (* = *)
| Ne (* Not equal, <> *)
| Lt (* Less than, < *)
| Le (* Less or equal, <= *)
| Gt (* Greater than, > *)
| Ge (* Greater or equal, >= *)

(** Condition : comparaison entre deux expressions *)
type cond = expr * comp * expr

(** Instructions *)
type instr =
  | Set of name * expr    (* := *)
  | Read of name
  | Print of expr
  | If of cond * block * block
  | While of cond * block
and block = (position * instr) list

(** Un programme Polish est un bloc d'instructions *)
type program = block

(** Possible signes *)
type sign = Neg | Zero | Pos | Error

(** Possible evaluations of signes list *)
type sign_bool = Yes | No | MayBe

(** List of signes *)
type signes = sign list

(** Environnement *)
module NameTable = Map.Make(String)

(** Set of variables *)
module Names = Set.Make(String)

(** Empty string *)
let empty_string : int = -1
