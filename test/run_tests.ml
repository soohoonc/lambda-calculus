open Lambda_calculus.Lexer

let test_lexer () =
  (* Identity function: λx.x *)
  print_endline "Testing identity function (\\x.x):";
  print_tokens (lexer "\\x.x");

  (* Basic application: (x y) *)
  print_endline "\nTesting basic application ((x y)):";
  print_tokens (lexer "(x y)");

  (* Y combinator: λf.(λx.f (x x)) (λx.f (x x)) *)
  print_endline "\nTesting Y combinator (\\f.(\\x.f (x x)) (\\x.f (x x))):";
  print_tokens (lexer "\\f.(\\x.f (x x)) (\\x.f (x x))")

let () = test_lexer ()
