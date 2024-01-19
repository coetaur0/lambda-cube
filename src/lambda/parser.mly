%{
open Ast

let rec nat_of_int location = function
  | 0 -> Term.{kind = Zero; location}
  | n -> Term.{kind = Succ (nat_of_int location (n - 1)); location}
%}

/* Identifiers. */
%token <string> NAME

/* Literals. */
%token NAT
%token <int> INT
%token BOOL
%token TRUE
%token FALSE

/* Keywords. */
%token LAMBDA
%token LET
%token IN
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED

/* Symbols. */
%token ARROW
%token EQ
%token NOT
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token COLON
%token COMMA
%token DOT
%token EOF

%start <Term.t> program

%type <Term.t> term
%type <Term.t> app
%type <Term.t> path
%type <Term.t> atom
%type <name * Term.t> field
%type <Type.t> ty
%type <Type.t> ty_atom
%type <name * Type.t> ty_field

%%

let program :=
  ~ = term; EOF; { term }

let term :=
  | LAMBDA; var = NAME; COLON; ~ = ty; DOT; body = term; { Term.{kind = Abs (var, ty, body); location = $loc} }
  | LET; var = NAME; EQ; value = term; IN; body = term;  { Term.{kind = Let (var, value, body); location = $loc} }
  | IF; cond = term; THEN; thn = term; ELSE; els = term; { Term.{kind = If (cond, thn, els); location = $loc} }
  | ~ = app;                                             { app }

let app := 
  | term = app; arg = path;    { Term.{kind = App (term, arg); location = $loc} }
  | lhs = app; EQ; rhs = path; { Term.{kind = Eq (lhs, rhs); location = $loc} }
  | NOT; term = path;          { Term.{kind = Not term; location = $loc} }
  | SUCC; term = path;         { Term.{kind = Succ term; location = $loc} }
  | PRED; term = path;         { Term.{kind = Pred term; location = $loc} }
  | ~ = path;                  { path }

let path :=
  | term = path; DOT; field = NAME; { Term.{kind = Project (term, field); location = $loc} }
  | ~ = atom;                       { atom }

let atom :=
  | LBRACE; fields = separated_list(COMMA, field); RBRACE; { Term.{kind = Record fields; location = $loc} }
  | var = NAME;                                            { Term.{kind = Variable var; location = $loc} }
  | int = INT;                                             { nat_of_int $loc int }
  | TRUE;                                                  { Term.{kind = True; location = $loc} }
  | FALSE;                                                 { Term.{kind = False; location = $loc} }
  | LPAREN; ~ = term; RPAREN;                              { term }

let field :=
  name = NAME; EQ; value = term; { (name, value) }

let ty :=
  | lhs = ty_atom; ARROW; rhs = ty; { Type.{kind = Arrow (lhs, rhs); location = $loc} }
  | ~ = ty_atom;                    { ty_atom }

let ty_atom :=
  | LBRACE; fields = separated_list(COMMA, ty_field); RBRACE; { Type.{kind = Record fields; location = $loc} }
  | NAT;                                                      { Type.{kind = Nat; location = $loc} }
  | BOOL;                                                     { Type.{kind = Bool; location = $loc} }
  | LPAREN; ~ = ty; RPAREN;                                   { ty }

let ty_field :=
  name = NAME; COLON; ~ = ty; { (name, ty) }
