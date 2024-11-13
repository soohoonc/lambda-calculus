(*
  the types for the lexer and parser
*)

type token =
  | LAMBDA
  | RPAREN
  | LPAREN
  | IDENTIFIER of string
  | DOT
  | EOF

type term =
  | Var of string (* variables *)
  | Abs of string * term (* abstraction *)
  | App of term * term (* application *)
