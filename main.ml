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
  let name = Filename.remove_extension filename in
  let outname = name ^ ".wat" in
  let ic = open_in filename in
  let oc = open_out outname in
  let l = Lexing.from_channel ic in
  assemble name oc l;
  print_endline ("compilation succeeded. wrote to " ^ outname);
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

let generate_dot_mode filename =
  let name = List.nth (String.split_on_char '.' filename) 0 in
  let ic = open_in filename in
  let l = Lexing.from_channel ic in
  let syntax = Parser.start Lexer.token l in
  let dotfile = name ^ "_ast.dot" in
  Visualizer.ast_to_dot syntax dotfile;
  close_in ic

let () =
  let argc = Array.length Sys.argv in
  if argc < 2 then (
    Printf.eprintf "While Lang Compiler\n\n";
    Printf.eprintf "Usage:\n";
    Printf.eprintf "  %s <filename.while>                  # Compile to WebAssembly\n" Sys.argv.(0);
    Printf.eprintf "  %s --visualize <filename.while>      # Show transformation steps (WASM)\n" Sys.argv.(0);
    Printf.eprintf "  %s -v <filename.while>               # Short form of --visualize\n" Sys.argv.(0);
    Printf.eprintf "  %s --dot <filename.while>            # Generate Graphviz DOT file for AST\n" Sys.argv.(0);
    Printf.eprintf "\nExamples:\n";
    Printf.eprintf "  %s test/assign.while\n" Sys.argv.(0);
    Printf.eprintf "  %s --visualize test/simple_loop.while\n" Sys.argv.(0);
    Printf.eprintf "  %s --dot test/loop.while\n" Sys.argv.(0);
    Printf.eprintf "  dot -Tpng test/loop_ast.dot -o ast.png  # Convert DOT to image\n";
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
    | "--dot" ->
        generate_dot_mode filename
    | _ ->
        Printf.eprintf "Unknown flag: %s\n" flag;
        Printf.eprintf "Use --visualize, --visualize-pyc, or --dot\n";
        exit 1
