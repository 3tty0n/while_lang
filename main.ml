let assemble filename oc l =
  Assemble_pyc.assemble oc
    (Virtual_pyc.compile filename
       (Parser.start Lexer.token l))

let main filename =
  let name = List.nth (String.split_on_char '.' filename) 0 in
  let outname = name ^ ".pyc" in
  let ic = open_in filename in
  let oc = open_out outname in
  let l = Lexing.from_channel ic in
  Assemble_pyc.assemble oc
    (Virtual_pyc.compile filename
       (Parser.start Lexer.token l));
  close_out oc

let () =
  let filename = Sys.argv.(1) in
  main filename
