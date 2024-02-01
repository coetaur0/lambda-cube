(* ----- Location ------------------------------------------------------------------------------- *)

type location = Lexing.position * Lexing.position

let pp_location fmt (lpos, rpos) =
  let open Lexing in
  let filename = lpos.pos_fname in
  let line = lpos.pos_lnum in
  let lchar = lpos.pos_cnum - lpos.pos_bol in
  let rchar = rpos.pos_cnum - lpos.pos_bol in
  Format.fprintf fmt "'%s' at line %d, characters %d-%d" filename line lchar rchar

(* ----- Syntax error --------------------------------------------------------------------------- *)

exception SyntaxError of location

(* ----- Language definition -------------------------------------------------------------------- *)

module type S = sig
  val name : string

  val semantics : string

  type ast

  val pp_ast : Format.formatter -> ast -> unit

  val parse : Lexing.lexbuf -> ast

  val translate : ast -> string

  val typecheck : string -> string * string

  val eval : string -> string * string
end

(* ----- Interpreter builder -------------------------------------------------------------------- *)

module Make (L : S) = struct
  open Elpi

  let parse path =
    In_channel.with_open_text path (fun file ->
        let src = In_channel.input_all file in
        let lexbuf = Lexing.from_string src in
        lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = path};
        try L.parse lexbuf
        with _ -> raise (SyntaxError (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)) )

  let run_elpi elpi program goal variable =
    try
      let goal = API.Parse.goal ~elpi ~loc:(API.Ast.Loc.initial "") ~text:goal in
      let query = API.Compile.query program goal in
      let exec = API.Compile.optimize query in
      match API.Execute.once exec with
      | API.Execute.Success result ->
        Some (API.Data.StrMap.find variable result.assignments, result.pp_ctx)
      | _ -> None
    with API.Compile.CompileError (loc, msg) -> API.Utils.error ?loc msg

  let exec filename =
    try
      let elpi =
        API.Setup.init ~legacy_parser:false ~builtins:[Builtin.std_builtins]
          ~file_resolver:(API.Parse.std_resolver ~paths:[] ())
          ()
      in
      let semantics = API.Compile.program ~elpi [API.Parse.program ~elpi ~files:[L.semantics]] in
      let program = L.translate (parse filename) in
      let (typecheck, typevar) = L.typecheck program in
      let (eval, evalvar) = L.eval program in
      match run_elpi elpi semantics typecheck typevar with
      | Some (ty, ty_pp_ctx) -> (
        match run_elpi elpi semantics eval evalvar with
        | Some (value, val_pp_ctx) ->
          Format.fprintf Format.std_formatter "%a : %a" (API.Pp.term val_pp_ctx) value
            (API.Pp.term ty_pp_ctx) ty
        | None -> print_endline "Runtime error." )
      | None -> print_endline "Type error."
    with SyntaxError location ->
      Format.fprintf Format.std_formatter "Syntax error in %a" pp_location location

  let run () =
    let filename =
      let open Command.Param in
      anon ("filename" %: string)
    in
    let command =
      Command.basic
        ~summary:(Printf.sprintf "Type check and execute a %s program" L.name)
        ~readme:(fun () -> "")
        (Command.Param.map filename ~f:(fun filename () -> exec filename))
    in
    Command_unix.run ~version:"0.1" command
end
