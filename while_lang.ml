let compile_pycode ic =
  let l = Lexing.from_channel ic in
  let oc = open_out ("out.pyc") in
  Assemble_pyc.assemble oc
    (Virtual_pyc.compile_and_create_pycode
       (Parser.start Lexer.token l))

let print_opcode s =
  let l = Lexing.from_string s in
  let opcode = Virtual_pyc.compile_pyc
      (Parser.start Lexer.token l) in
  Virtual_pyc.print_pyc_list opcode

let test_pycode fname s =
  let l = Lexing.from_string s in
  let oc = open_out fname in
  Assemble_pyc.assemble oc
    (Virtual_pyc.compile_and_create_pycode
       (Parser.start Lexer.token l))

let test () =
  test_pycode "assign.pyc" "i := 1; i := i + 1; print i;";
  test_pycode "loop.pyc" "i := 1; while i < 10 do begin i := i + 1; end; print i;"

let () =
  test ()
