(* open Types *)

(* let parser tokens =
  let rec parser_aux tokens =
    match tokens with
    | LAMBDA :: rest -> ()
    | LPAREN :: rest -> ()
    | IDENTIFIER s :: rest -> ()
    | EOF :: tokens -> ()
    | [] -> failwith "Error: Unexpected end"
    | _ -> failwith "Error: Invalid Token"
  in
  parser_aux tokens *)
