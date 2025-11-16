let assemble name oc l =
  let syntax = Parser.start Lexer.token l in
  let ops_stack = Virtual_stack.compile_stack syntax in
  let ops_pyc = Emit_pyc.compile_pyc ops_stack in
  (* Pycode.print_pyc_list ops_pyc; *)
  let pycode = Emit_pyc.compile name ops_stack in
  Assemble_pyc.assemble oc pycode

let assemble_wasm name oc l =
  let syntax = Parser.start Lexer.token l in
  let ops_stack = Virtual_stack.compile_stack syntax in
  Emit_wasm.compile_expr_subset stdout ops_stack

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
  assemble_wasm name oc l;
  close_out oc

let test () =
  let s1 = "while i < 10 do i := i + 1;" in
  print_stack_code (Lexing.from_string s1)

let visualize_mode filename output_format =
  let name = List.nth (String.split_on_char '.' filename) 0 in
  let ic = open_in filename in
  let l = Lexing.from_channel ic in
  let syntax = Parser.start Lexer.token l in
  let ops_stack = Virtual_stack.compile_stack syntax in
  Visualizer.visualize_transformations filename syntax ops_stack output_format;
  Visualizer.print_summary ();
  close_in ic

let () =
  let argc = Array.length Sys.argv in
  if argc < 2 then (
    Printf.eprintf "While Lang Compiler\n\n";
    Printf.eprintf "Usage:\n";
    Printf.eprintf "  %s <filename.while>                # Compile to WebAssembly\n" Sys.argv.(0);
    Printf.eprintf "  %s --visualize <filename.while>    # Show transformation steps (WASM)\n" Sys.argv.(0);
    Printf.eprintf "  %s -v <filename.while>             # Short form of --visualize\n" Sys.argv.(0);
    Printf.eprintf "  %s --visualize-pyc <filename.while> # Show transformation steps (Python)\n" Sys.argv.(0);
    Printf.eprintf "\nExamples:\n";
    Printf.eprintf "  %s test/assign.while\n" Sys.argv.(0);
    Printf.eprintf "  %s --visualize test/simple_loop.while\n" Sys.argv.(0);
    exit 1
  ) else if argc = 2 then
    (* Simple compilation mode *)
    main Sys.argv.(1)
  else
    (* Check for visualization flags *)
    let flag = Sys.argv.(1) in
    let filename = Sys.argv.(2) in
    match flag with
    | "--visualize" | "-v" ->
        visualize_mode filename "wasm"
    | "--visualize-pyc" | "-vp" ->
        visualize_mode filename "pyc"
    | _ ->
        Printf.eprintf "Unknown flag: %s\n" flag;
        Printf.eprintf "Use --visualize or -v for visualization mode\n";
        exit 1
