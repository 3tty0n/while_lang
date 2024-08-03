let assemble name oc l =
  let syntax = Parser.start Lexer.token l in
  let ops_stack = Virtual_stack.compile_stack syntax in
  let ops_pyc = Emit_pyc.compile_pyc ops_stack in
  Pycode.print_pyc_list ops_pyc;
  let pycode = Emit_pyc.compile name ops_stack in
  Assemble_pyc.assemble oc pycode

let print_stack_code l =
  Virtual_stack.print_code stdout
    (Virtual_stack.compile_stack
       (Parser.start Lexer.token l))

let string name s =
  let oc = open_out name in
  assemble "test.while" oc (Lexing.from_string s);
  close_out oc

let main filename =
  let name = List.nth (String.split_on_char '.' filename) 0 in
  let outname = name ^ ".pyc" in
  let ic = open_in filename in
  let oc = open_out outname in
  let l = Lexing.from_channel ic in
  assemble name oc l;
  close_out oc

let test () =
  let s1 = "while i < 10 do i := i + 1;" in
  print_stack_code (Lexing.from_string s1)

let () =
  if (Array.length Sys.argv) > 1 then
    let filename = Sys.argv.(1) in
    main filename
  else (
    Printf.eprintf "[usage] %s filename.while\n" Sys.argv.(0);
    test ();
    exit(1)
  )
