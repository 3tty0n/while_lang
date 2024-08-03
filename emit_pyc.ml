open Virtual_stack
open Pycode

let count_varenv = ref (-1)
let count_constenv = ref (-1)

let reset _ =
  count_varenv := (-1);
  count_constenv := (-1)

let find_varenv id (varenv : (w_object * int) list) =
  try List.assoc id varenv, varenv
  with Not_found ->
    incr count_varenv;
    let varenv = (id, !count_varenv) :: varenv in
    !count_varenv, varenv

let find_constenv n constenv =
  try List.assoc n constenv, constenv
  with Not_found ->
    incr count_constenv;
    let constenv = (n, !count_constenv) :: constenv in
    !count_constenv, constenv


let rec compile_stack_pyc varenv constenv aux ops =
  let open Virtual_stack in
  match ops with
  | [] -> aux, varenv, constenv
  | op :: tl ->
    (match op with
     | LPush s ->
       let regnum, varenv = find_varenv (W_String s) varenv in
       compile_stack_pyc varenv constenv (aux @ [STORE_NAME regnum]) tl
     | RValue s ->
       let regnum, varenv = find_varenv (W_String s) varenv in
       compile_stack_pyc varenv constenv (aux @ [LOAD_NAME regnum]) tl
     | Push n ->
       let regnum, constenv = find_constenv (W_Int n) constenv in
       compile_stack_pyc varenv constenv (aux @ [LOAD_CONST regnum]) tl
     | PLUS -> compile_stack_pyc varenv constenv (aux @ [BINARY_ADD]) tl
     | MINUS -> compile_stack_pyc varenv constenv (aux @ [BINARY_SUBTRACT]) tl
     | TIMES -> compile_stack_pyc varenv constenv (aux @ [BINARY_MULTIPLY]) tl
     | DIV -> compile_stack_pyc varenv constenv (aux @ [BINARY_TRUE_DIVIDE]) tl
     | NOT -> compile_stack_pyc varenv constenv (aux @ [UNARY_NOT]) tl
     | AND -> compile_stack_pyc varenv constenv (aux @ [BINARY_AND]) tl
     | OR -> compile_stack_pyc varenv constenv (aux @ [BINARY_OR]) tl
     | TRUE ->
       let reg_num, varenv = find_varenv (W_String "True") varenv in
       compile_stack_pyc varenv constenv (aux @ [LOAD_NAME reg_num]) tl
     | FALSE ->
       let reg_num, varenv = find_varenv (W_String "False") varenv in
       compile_stack_pyc varenv constenv (aux @ [LOAD_NAME reg_num]) tl
     | EQ -> compile_stack_pyc varenv constenv (aux @ [COMPARE_OP 2]) tl
     | LT -> compile_stack_pyc varenv constenv (aux @ [COMPARE_OP 0]) tl
     | LE -> compile_stack_pyc varenv constenv (aux @ [COMPARE_OP 1]) tl
     | GT -> compile_stack_pyc varenv constenv (aux @ [COMPARE_OP 4]) tl
     | GE -> compile_stack_pyc varenv constenv (aux @ [COMPARE_OP 5] ) tl
     | PRINT -> compile_stack_pyc varenv constenv (aux @ [PRINT_ITEM; PRINT_NEWLINE]) tl
     | LabelTest (l, m) -> compile_stack_pyc varenv constenv (aux @ [Label_top (l, m)]) tl
     | LabelOut (l, m) -> compile_stack_pyc varenv constenv (aux @ [Label_out m]) tl
     | GoTo l -> compile_stack_pyc varenv constenv (aux @ [Jump l] ) tl
     | GoFalse l -> compile_stack_pyc varenv constenv (aux @ [Jump_if l]) tl)

let generate_pycode name varenv constenv pyc =
  let w_none_reg_num = !count_constenv + 1 in
  let const_tuple = List.map fst (constenv @ [(W_None, w_none_reg_num)]) in
  let var_tuple = List.map fst varenv in
  let code = create_bytes_pyc (pyc @ [LOAD_CONST w_none_reg_num; RETURN_VALUE]) in
  PyCode (
    { argcount=0; nlocals=4; stacksize=4; flags=64;
      code=code; consts=const_tuple; names=var_tuple; varnames=[];
      freevars=[]; cellvars=[]; name=name; filename="<module>";
      firstlineno=0; lnotab=(Bytes.of_string "4")
    }
  )

let compile_stack ops =
    reset ();
    let pyc, varenv, constenv = compile_stack_pyc [] [] [] ops in
    resolve_label pyc

let compile name ops =
    reset ();
    let pyc, varenv, constenv = compile_stack_pyc [] [] [] ops in
    let varenv, constenv = List.rev varenv, List.rev constenv in
    let pyc = resolve_label pyc in
    generate_pycode name varenv constenv pyc
