open Types

let is_whitespace s =
  match s with ' ' | '\n' | '\t' | '\r' -> true | _ -> false

let is_identifier s =
  match s with 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let lexer input =
  let rec scanner index tokens =
    if index >= String.length input then EOF :: tokens
    else
      let c = input.[index] in
      match c with
      | '\\' -> scanner (index + 1) (LAMBDA :: tokens)
      | '.' -> scanner (index + 1) (DOT :: tokens)
      | '(' -> scanner (index + 1) (LPAREN :: tokens)
      | ')' -> scanner (index + 1) (RPAREN :: tokens)
      | c when is_whitespace c -> scanner (index + 1) tokens
      | c when is_identifier c ->
          let rec get_identifier identifier i =
            if i >= String.length input || not (is_identifier input.[i]) then
              (identifier, i)
            else get_identifier (identifier ^ String.make 1 input.[i]) (i + 1)
          in
          let identifier, end_i =
            get_identifier (String.make 1 c) (index + 1)
          in
          scanner end_i (IDENTIFIER identifier :: tokens)
      | _ -> failwith "Error: Unexpected character"
  in
  List.rev (scanner 0 [])