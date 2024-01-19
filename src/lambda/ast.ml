(** Abstract syntax tree (AST) module. *)

open Cube

(** A name in the AST. *)
type name = string [@@deriving show]

(** Simply typed lambda calculus types. *)
module Type = struct
  (** A type expression. *)
  type t =
    { kind : kind;
      location : Language.location [@opaque] }
  [@@deriving show]

  (** The kind of a type expression. *)
  and kind =
    | Arrow of t * t
    | Record of (name * t) list
    | Nat
    | Bool
  [@@deriving show]
end

(** Simply typed lambda calculus terms. *)
module Term = struct
  (** A term expression. *)
  type t =
    { kind : kind;
      location : Language.location [@opaque] }
  [@@deriving show]

  (** The kind of a term expression. *)
  and kind =
    | Abs of name * Type.t * t
    | App of t * t
    | Record of (name * t) list
    | Project of t * name
    | Let of name * t * t
    | If of t * t * t
    | Variable of name
    | Eq of t * t
    | Not of t
    | Succ of t
    | Pred of t
    | True
    | False
    | Zero
  [@@deriving show]
end
