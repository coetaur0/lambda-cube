(** Language definition module. *)

(* ----- Location ------------------------------------------------------------------------------- *)

(** A location in a source. *)
type location = Lexing.position * Lexing.position

(** [pp_location fmt location] pretty prints a [location] using some formatter [fmt]. *)
val pp_location : Format.formatter -> location -> unit

(* ----- Language definition -------------------------------------------------------------------- *)

(** A language definition. *)
module type S = sig
  (** The name of the language. *)
  val name : string

  (** The path to the λ-Prolog file implementing the static and dynamic semantics for the language. *)
  val semantics : string

  (** The type of the abstract syntax tree (AST) of the language. *)
  type ast

  (** [pp_ast fmt ast] pretty prints an [ast] using some formatter [fmt]. *)
  val pp_ast : Format.formatter -> ast -> unit

  (** [parse lexbuf] is the AST parsed from some [lexbuf]. *)
  val parse : Lexing.lexbuf -> ast

  (** [translate ast] is the translation of an AST to its λ-Prolog representation. *)
  val translate : ast -> string

  (** [typecheck program] is the query built for some λ-Prolog representation of a [program] to
      typecheck it, along with the name of the variable that will be unified with the solution. *)
  val typecheck : string -> string * string

  (** [eval program] is the query built for some λ-Prolog representation of a [program] to evaluate
      it, along with the name of the variable that will be unified with the solution. *)
  val eval : string -> string * string
end

(* ----- Interpreter builder -------------------------------------------------------------------- *)

(** Builds an interpreter for a language, given its definition. *)
module Make : functor (_ : S) -> sig
  (** [run ()] executes the interpreter. *)
  val run : unit -> unit
end
