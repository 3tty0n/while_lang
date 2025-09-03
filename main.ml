let assemble name oc l =
  let syntax = Parser.start Lexer.token l in
  let ops_stack = Virtual_stack.compile_stack syntax in
  Emit_wasm.compile oc ops_stack

let print_stack_code l =
  Virtual_stack.print_code stdout
    (Virtual_stack.compile_stack
       (Parser.start Lexer.token l))

let string name s =
  let oc = open_out name in
  assemble "test.while" oc (Lexing.from_string s);
  close_out oc

let main filename =
  print_endline ("compiling " ^ filename ^ " ...");
  let name = List.nth (String.split_on_char '.' filename) 0 in
  let outname = name ^ ".wat" in
  let ic = open_in filename in
  let oc = open_out outname in
  let l = Lexing.from_channel ic in
  assemble name oc l;
  print_endline ("compilation succeeded. wrote to " ^ outname);
  close_out oc

let () =
  if (Array.length Sys.argv) > 1 then
    let filename = Sys.argv.(1) in
    main filename
  else (
    Printf.eprintf "[usage] %s filename.while\n" Sys.argv.(0);
    exit(1)
  )
