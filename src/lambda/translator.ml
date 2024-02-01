(** AST to λ-Prolog translation module. *)

(** [emit_fields emit fields] is the translation of a list of record [fields] to λ-Prolog, using
    some function [emit] to translate each field. *)
let emit_fields emit fields =
  List.fold_right
    (fun (field, value) repr -> Printf.sprintf "(pr %s %s) :: %s" field (emit value) repr)
    fields "nil"

(** [emit_type ty] is the translation of a simply typed λ-calculus type to a λ-Prolog term. *)
let rec emit_type ty =
  match Ast.Type.(ty.kind) with
  | Arrow (lhs, rhs) -> Printf.sprintf "(arrow %s %s)" (emit_type lhs) (emit_type rhs)
  | Record fields -> Printf.sprintf "(recty (%s))" (emit_fields emit_type fields)
  | Nat -> "nat"
  | Bool -> "bool"

(** [emit_term term] is the translation of a simply typed λ-calculus term to a λ-Prolog term. *)
let rec emit_term term =
  match Ast.Term.(term.kind) with
  | Abs (var, ty, body) -> Printf.sprintf "(abs %s (%s\\ %s))" (emit_type ty) var (emit_term body)
  | App (term, arg) -> Printf.sprintf "(app %s %s)" (emit_term term) (emit_term arg)
  | Record fields -> Printf.sprintf "(rec (%s))" (emit_fields emit_term fields)
  | Project (term, field) -> Printf.sprintf "(proj %s %s)" (emit_term term) field
  | Let (var, value, body) ->
    Printf.sprintf "(let %s (%s\\ %s))" (emit_term value) var (emit_term body)
  | If (cond, thn, els) ->
    Printf.sprintf "(if %s %s %s)" (emit_term cond) (emit_term thn) (emit_term els)
  | Variable var -> var
  | Eq (lhs, rhs) -> Printf.sprintf "(eq %s %s)" (emit_term lhs) (emit_term rhs)
  | Not term -> Printf.sprintf "(nt %s)" (emit_term term)
  | Succ term -> Printf.sprintf "(succ %s)" (emit_term term)
  | Pred term -> Printf.sprintf "(pre %s)" (emit_term term)
  | True -> "tt"
  | False -> "ff"
  | Zero -> "zero"
