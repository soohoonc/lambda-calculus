open Types

(*
  the parser (left associative)
  input:
    tokens: list of tokens
  output:
    term: the parsed term
*)
let rec term_to_string term =
  match term with
  | Var s -> s
  | Abs (s, t) -> "(\\" ^ s ^ "." ^ term_to_string t ^ ")"
  | App (t1, t2) -> "(" ^ term_to_string t1 ^ " " ^ term_to_string t2 ^ ")"

let print_term term =
  print_endline (term_to_string term)

let parser tokens =
  (* Mutable reference to track current position in token list *)
  let current_pos = ref 0 in
  let tokens_array = Array.of_list tokens in
  
  (* Helper to peek at current token *)
  let peek () =
    if !current_pos >= Array.length tokens_array then EOF
    else tokens_array.(!current_pos)
  in
  
  (* Helper to advance to next token *)
  let next () =
    if !current_pos < Array.length tokens_array then
      current_pos := !current_pos + 1
  in
  
  (* Helper to match and consume expected token *)
  let expect token =
    if peek () = token then
      (next (); true)
    else false
  in
  
  (* Parse a single variable or parenthesized expression *)
  let rec parse_atom () =
    match peek () with
    | IDENTIFIER s -> 
        next ();
        Var s
    | LPAREN ->
        next ();
        let term = parse_term () in
        if not (expect RPAREN) then
          raise (Failure "Expected right parenthesis");
        term
    | _ -> raise (Failure "Expected identifier or left parenthesis")
  
  (* Parse a sequence of applications *)
  and parse_apps () =
    let rec loop left =
      match peek () with
      | IDENTIFIER _ | LPAREN ->
          let right = parse_atom () in
          loop (App (left, right))
      | _ -> left
    in
    let left = parse_atom () in
    loop left
  
  (* Parse a complete term *)
  and parse_term () =
    match peek () with
    | LAMBDA ->
        next ();
        (match peek () with
        | IDENTIFIER param ->
            next ();
            if not (expect DOT) then
              raise (Failure "Expected dot after parameter in lambda");
            let body = parse_term () in
            Abs (param, body)
        | _ -> raise (Failure "Expected identifier after lambda"))
    | _ -> parse_apps ()
  in
  
  (* Start parsing from the first token *)
  let result = parse_term () in
  if !current_pos < Array.length tokens_array - 1 then
    raise (Failure "Unexpected tokens after valid expression")
  else
    result
