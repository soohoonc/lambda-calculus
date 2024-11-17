open Types


let token_to_string token =
  match token with
  | LAMBDA -> "LAMBDA"
  | DOT -> "DOT"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | IDENTIFIER s -> "IDENTIFIER(" ^ s ^ ")"
  | EOF -> "EOF"

let print_tokens tokens =
  List.iter
    (fun token ->
      print_string (token_to_string token);
      print_string "; ")
    tokens;
  print_newline ()


let rec term_to_string term =
  match term with
  | Var s -> s
  | Abs (s, t) -> "(\\" ^ s ^ "." ^ term_to_string t ^ ")"
  | App (t1, t2) -> "(" ^ term_to_string t1 ^ " " ^ term_to_string t2 ^ ")"

let print_term term =
  print_endline (term_to_string term)