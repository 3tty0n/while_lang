let string s =
  let l = Lexing.from_string s in
  Emit.compile_statement (Parser.start Lexer.token l)

let test s =
  Emit.print_code stdout (string s)

let () =
  let s1 = "begin a := a * 2; i := i + 1; end;" in
  test s1;
  let s2 = "while i < 10 do i := i + 1;" in
  test s2;
  ()
