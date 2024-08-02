let string s =
  let l = Lexing.from_string s in
  Emit_pyc.compile_pyc
    (Ssa.assign_id
       (Parser.start Lexer.token l))

let test s =
  string s

let () =
  (* test "i := 1; a := 1; begin a := a * 2; i := i + 1; end;"; *)
  (* print_newline (); *)
  test "i:= 0; while i < 10 do i := i + 1;";
  ()
