let string s =
  let l = Lexing.from_string s in
  Emit.compile_statement (Parser.start Lexer.token l)

let () =
  let s1 = "a := a * 1" in
  let result = string s1 in
  Emit.print_code result;
  ()
