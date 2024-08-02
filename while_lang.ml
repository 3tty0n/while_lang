let string s =
  let l = Lexing.from_string s in
  Emit_stackmachine.compile_statement
    (Ssa.assign_id
       (Parser.start Lexer.token l))

let test s =
  Emit_stackmachine.print_code stdout (string s)

let test_ssa s =
  let l = Lexing.from_string s in
  let s, _ = Ssa.assign_id_statement (Parser.start Lexer.token l) Ssa.M.empty in
  print_endline (Syntax.string_of_statement s)

let () =
  test_ssa "a := a + 1; b := a + 1; c := b + 1; a := a + b;";

  test "i := 1; a := 1; begin a := a * 2; i := i + 1; end;";
  print_newline ();
  test "i:= 0; while i < 10 do i := i + 1;";
  ()
