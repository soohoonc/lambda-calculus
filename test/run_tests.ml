open Lambda_calculus.Lexer
open Lambda_calculus.Parser
open Lambda_calculus.Eval
let test_lexer () = (
  print_endline "Testing lexer:";
  print_endline "Testing empty string:";
  print_tokens (lexer "");
  (* Identity function: λx.x *)
  print_endline "\nTesting identity function (\\x.x):";
  print_tokens (lexer "\\x.x");
  (* Basic application: (x y) *)
  print_endline "\nTesting basic application ((x y)):";
  print_tokens (lexer "(x y)");
  (* Y combinator: λf.(λx.f (x x)) (λx.f (x x)) *)
  print_endline "\nTesting Y combinator (\\f.(\\x.f (x x)) (\\x.f (x x))):";
  print_tokens (lexer "\\f.(\\x.f (x x)) (\\x.f (x x))");
  print_endline "\n";)

let () = test_lexer ()

let test_parser () = (
  print_endline "Testing parser:";
  print_endline "\nTesting empty string:";
  try
    print_term (parser (lexer ""));
    print_endline "Error: Expected exception was not raised"
  with
    Failure msg -> print_endline ("Success: Caught expected error - " ^ msg);
  print_endline "\nTesting identity function (\\x.x):";
  print_term (parser (lexer "\\x.x"));
  print_endline "\nTesting basic application ((x y)):";
  print_term (parser (lexer "(x y)"));
  print_endline "\nTesting Y combinator (\\f.(\\x.f (x x)) (\\x.f (x x))):";
  print_term (parser (lexer "\\f.(\\x.f (x x)) (\\x.f (x x))"));
  print_endline "\nTesting the Y combinator (ambiguous) \\f.\\x.f (x x) \\x.f (x x):";
  print_term (parser (lexer "\\f.\\x.f (x x) \\x.f (x x)"));
  print_endline "\n";)

let () = test_parser ()

let test_eval () = (
  print_endline "Testing eval:";
  print_endline "\nTesting identity function (\\x.x):";
  print_term (eval (parser (lexer "\\x.x")));
  print_endline "\nTesting basic application ((x y)):";
  print_term (eval (parser (lexer "(x y)")));
  print_endline "\nTesting Y combinator (\\f.(\\x.f (x x)) (\\x.f (x x))):";
  print_term (eval (parser (lexer "\\f.(\\x.f (x x)) (\\x.f (x x))")));
  print_endline "\n";)

let () = test_eval ()
