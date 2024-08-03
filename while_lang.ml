let compile_pycode ic =
  let l = Lexing.from_channel ic in
  let oc = open_out ("out.pyc") in
  Assemble_pyc.assemble oc
    (Emit_pyc.compile_and_create_pycode
       (Parser.start Lexer.token l))

let string_pycode s =
  let l = Lexing.from_string s in
  let oc = open_out ("out.pyc") in
  Assemble_pyc.assemble oc
    (Emit_pyc.compile_and_create_pycode
       (Parser.start Lexer.token l))

let print_opcode s =
  let l = Lexing.from_string s in
  let opcode = Emit_pyc.compile_pyc
      (Parser.start Lexer.token l) in
  List.iter Emit_pyc.print_pyc opcode

let test_pycode s =
  string_pycode s

let () =
  (* test "i := 1; a := 1; begin a := a * 2; i := i + 1; end;"; *)
  print_opcode "i := 1; i := i + 1; print i;";
  test_pycode "i := 1; i := i + 1; print i;"
