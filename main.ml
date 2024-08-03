let assemble name oc l =
  Assemble_pyc.assemble oc
    (Emit_pyc.compile name
       (Virtual_stack.compile_stack
          (Parser.start Lexer.token l)))

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
  string "assign.pyc" "x := 1; x := x + 1; print x;";
  string "loop.pyc" "i := 1; while i < 10 do begin i := i + 1; end; print i;"

let () =
  if (Array.length Sys.argv) > 1 then
    let filename = Sys.argv.(1) in
    main filename
  else (
    Printf.eprintf "[usage] %s filename.while\n" Sys.argv.(0);
    exit(1)
  )
