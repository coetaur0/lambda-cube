open Cube

(** An interpreter for the simply typed λ-calculus. *)
module Interpreter = Language.Make (struct
  let name = "simply typed λ-calculus"

  let semantics = "./src/lambda/lambda.elpi"

  type ast = Ast.Term.t

  let pp_ast = Ast.Term.pp

  let parse = Parser.program Lexer.token

  let translate ast = Translator.emit_term ast

  let typecheck program = (Printf.sprintf "typeof %s T." program, "T")

  let eval program = (Printf.sprintf "eval %s V." program, "V")
end)

let () = Interpreter.run ()
