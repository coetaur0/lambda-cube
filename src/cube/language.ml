(* ----- Location ------------------------------------------------------------------------------- *)

type location = Lexing.position * Lexing.position

let pp_location fmt (lpos, rpos) =
  let open Lexing in
  let filename = lpos.pos_fname in
  let line = lpos.pos_lnum in
  let lchar = lpos.pos_cnum - lpos.pos_bol in
  let rchar = rpos.pos_cnum - lpos.pos_bol in
  Format.fprintf fmt "'%s' at line %d, characters %d-%d" filename line lchar rchar

(* ----- Error ---------------------------------------------------------------------------------- *)

type error_kind = SyntaxError

exception Error of error_kind * location

let pp_error fmt (kind, location) =
  match kind with
  | SyntaxError -> Format.fprintf fmt "Syntax error in %a." pp_location location

(* ----- Language definition -------------------------------------------------------------------- *)

module type S = sig
  val name : string

  type ast

  val pp_ast : Format.formatter -> ast -> unit

  val parse : Lexing.lexbuf -> ast
end

(* ----- Interpreter builder -------------------------------------------------------------------- *)

module Make (L : S) = struct
  let parse filename src =
    let lexbuf = Lexing.from_string src in
    lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = filename};
    try L.parse lexbuf
    with _ ->
      raise (Error (SyntaxError, (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)))

  let parse_file path =
    In_channel.with_open_text path (fun file ->
        let src = In_channel.input_all file in
        parse path src )

  let exec filename =
    try L.pp_ast Format.std_formatter (parse_file filename)
    with Error (kind, location) -> pp_error Format.std_formatter (kind, location)

  let run () =
    let filename =
      let open Command.Param in
      anon ("filename" %: string)
    in
    let command =
      Command.basic
        ~summary:(Printf.sprintf "Parse a %s program" L.name)
        ~readme:(fun () -> "More detailed information")
        (Command.Param.map filename ~f:(fun filename () -> exec filename))
    in
    Command_unix.run ~version:"0.1" command
end
