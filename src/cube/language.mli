(** Language definition module. *)

(** A location in a source. *)
type location = Lexing.position * Lexing.position

(** [pp_location fmt location] pretty prints a [location] using some formatter [fmt]. *)
val pp_location : Format.formatter -> location -> unit

(** A language definition. *)
module type S = sig
  (** The name of the language. *)
  val name : string

  (** The type of abstract syntax trees (AST) in the language. *)
  type ast

  (** [pp_ast fmt ast] pretty prints an [ast] using some formatter [fmt]. *)
  val pp_ast : Format.formatter -> ast -> unit

  (** [parse lexbuf] is the AST parsed from some [lexbuf]. *)
  val parse : Lexing.lexbuf -> ast
end

(** Builds an interpreter for a language, given its definition. *)
module Make : functor (_ : S) -> sig
  (** [run ()] executes the interpreter. *)
  val run : unit -> unit
end
