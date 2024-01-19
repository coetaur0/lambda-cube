open Cube

module Interpreter = Language.Make (struct 
  let name = "λ-calculus"

  type ast = Ast.Term.t

  let pp_ast = Ast.Term.pp

  let parse = Parser.program Lexer.token
end)

let () = Interpreter.run ()
