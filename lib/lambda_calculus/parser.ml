open Types

(*
  the parser (left associative)
  input:
    tokens: list of tokens
  output:
    term: the parsed term
*)
let parser tokens =
  let peek = function
    | [] -> EOF
    | t :: _ -> t
  in
  
  let next = function
    | [] -> []
    | _ :: rest -> rest
  in
  
  (* Parse a single variable or parenthesized expression *)
  let rec parse_atom tokens =
    match peek tokens with
    | IDENTIFIER s -> 
        (Var s, next tokens)
    | LPAREN ->
        (let (term, tokens') = parse_term (next tokens) in
        match peek tokens' with
        | RPAREN -> (term, next tokens')
        | _ -> raise (Failure "Expected right parenthesis"))
    | _ -> raise (Failure "Expected identifier or left parenthesis")
  
  (* Parse a sequence of applications *)
  and parse_apps tokens =
    let rec loop left tokens =
      match peek tokens with
      | IDENTIFIER _ | LPAREN ->
          let (right, tokens') = parse_atom tokens in
          loop (App (left, right)) tokens'
      | LAMBDA ->
          let (right, tokens') = parse_term tokens in
          loop (App (left, right)) tokens'
      | _ -> (left, tokens)
    in
    let (left, tokens') = parse_atom tokens in
    loop left tokens'
  
  (* Parse a complete term *)
  and parse_term tokens =
    match peek tokens with
    | LAMBDA -> (* If a lambda then we are dealing with an abstraction *)
        (match peek (next tokens) with
        | IDENTIFIER param ->
            let tokens' = next (next tokens) in
            if peek tokens' <> DOT then
              raise (Failure "Expected dot after parameter in lambda");
            let (body, tokens'') = parse_term (next tokens') in
            (Abs (param, body), tokens'')
        | _ -> raise (Failure "Expected identifier after lambda"))
    | _ -> parse_apps tokens (* If not a lambda then we are dealing with an application *)
  in
  
  (* Start parsing from the first token *)
  let (ast, remaining) = parse_term tokens in
  match remaining with
  | [] | [EOF] -> ast
  | _ -> 
      raise (Failure ("Unexpected tokens after valid expression"))
