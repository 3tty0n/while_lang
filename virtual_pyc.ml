open Syntax
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
    let constenv =  (n, !count_constenv) :: constenv in
    !count_constenv, constenv

let rec compile_arith_pyc a varenv constenv =
  match a with
  | Var id ->
    let reg_num, varenv = find_varenv (W_String id) varenv  in
    [LOAD_NAME reg_num], varenv, constenv
  | Num n ->
    let reg_num, constenv = find_constenv (W_Int n) constenv in
    [LOAD_CONST reg_num], varenv, constenv
  | Add (lhs, rhs) ->
    let pyc1, varenv, constenv = compile_arith_pyc lhs varenv constenv in
    let pyc2, varenv, constenv = compile_arith_pyc rhs varenv constenv in
    pyc1 @ pyc2 @ [BINARY_ADD], varenv, constenv
  | Sub (lhs, rhs) ->
    let pyc1, varenv, constenv = compile_arith_pyc lhs varenv constenv in
    let pyc2, varenv, constenv = compile_arith_pyc rhs varenv constenv in
    pyc1 @ pyc2 @ [BINARY_ADD], varenv, constenv
  | Mul (lhs, rhs) ->
    let pyc1, varenv, constenv = compile_arith_pyc lhs varenv constenv in
    let pyc2, varenv, constenv = compile_arith_pyc rhs varenv constenv in
    pyc1 @ pyc2 @ [BINARY_MULTIPLY], varenv, constenv
  | Div (lhs, rhs) ->
    let pyc1, varenv, constenv = compile_arith_pyc lhs varenv constenv in
    let pyc2, varenv, constenv = compile_arith_pyc rhs varenv constenv in
    pyc1 @ pyc2 @ [BINARY_TRUE_DIVIDE], varenv, constenv


(* cmp_op: ('<', '<=', '==', '!=', '>', '>=', 'in', 'not in', 'is', 'is not', 'exception match', 'BAD') *)
let rec compile_pred_pyc p varenv constenv =
  match p with
  | True ->
    let reg_num, varenv = find_varenv (W_String "True") varenv in
    [LOAD_NAME reg_num], varenv, constenv
  | False ->
    let reg_num, varenv = find_varenv (W_String "False") varenv in
    [LOAD_NAME reg_num], varenv, constenv
  | Not p ->
    let pyc, varenv, constenv = compile_pred_pyc p varenv constenv in
    pyc @ [UNARY_NOT], varenv, constenv
  | And (p1, p2) ->
    let py1, varenv, constenv = compile_pred_pyc p1 varenv constenv in
    let py2, varenv, constenv = compile_pred_pyc p2 varenv constenv in
    py1 @ py2 @ [BINARY_AND], varenv, constenv
  | Or (p1, p2) ->
    let py1, varenv, constenv = compile_pred_pyc p1 varenv constenv in
    let py2, varenv, constenv = compile_pred_pyc p2 varenv constenv in
    py1 @ py2 @ [BINARY_OR], varenv, constenv
  | LT (a1, a2) ->
    let py1, varenv, constenv = compile_arith_pyc a1 varenv constenv in
    let py2, varenv, constenv = compile_arith_pyc a2 varenv constenv in
    py1 @ py2 @ [COMPARE_OP 0], varenv, constenv
  | LE (a1, a2) ->
    let py1, varenv, constenv = compile_arith_pyc a1 varenv constenv in
    let py2, varenv, constenv = compile_arith_pyc a2 varenv constenv in
    py1 @ py2 @ [COMPARE_OP 1], varenv, constenv
  | GT (a1, a2) ->
    let py1, varenv, constenv = compile_arith_pyc a1 varenv constenv in
    let py2, varenv, constenv = compile_arith_pyc a2 varenv constenv in
    py1 @ py2 @ [COMPARE_OP 4], varenv, constenv
  | GE (a1, a2) ->
    let py1, varenv, constenv = compile_arith_pyc a1 varenv constenv in
    let py2, varenv, constenv = compile_arith_pyc a2 varenv constenv in
    py1 @ py2 @ [COMPARE_OP 5], varenv, constenv
  | EQ (a1, a2) ->
    let py1, varenv, constenv = compile_arith_pyc a1 varenv constenv in
    let py2, varenv, constenv = compile_arith_pyc a2 varenv constenv in
    py1 @ py2 @ [COMPARE_OP 2], varenv, constenv

let label_counter = ref (-1)
let gen_label () =
  incr label_counter;
  "L." ^ (string_of_int !label_counter)

let rec compile_statement_pyc s (varenv : (w_object * int) list) (constenv : (w_object * int) list) =
  match s with
  | Print (a) ->
    let pyc, varenv, constenv  = compile_arith_pyc a varenv constenv in
    pyc @ [PRINT_ITEM; PRINT_NEWLINE], varenv, constenv
  | Assign (id, a) ->
    let pyc, varenv, constenv = compile_arith_pyc a varenv constenv in
    let reg_num, varenv = find_varenv (W_String id) varenv in
    pyc @ [STORE_NAME reg_num], varenv, constenv
  | Seq (s1, s2) ->
    let pyc1, varenv, constenv = compile_statement_pyc s1 varenv constenv in
    let pyc2, varenv, constenv = compile_statement_pyc s2 varenv constenv in
    pyc1 @ pyc2, varenv, constenv
  | While (p, s) ->
    let pyc1, varenv, constenv= compile_pred_pyc p varenv constenv  in
    let pyc2, varenv, constenv = compile_statement_pyc s varenv constenv in
    let l_top = gen_label () in
    let l_out = gen_label () in
    [Label_top (l_top, l_out); ] @ pyc1 @ [Jump_if l_out] @ pyc2 @ [Jump l_top] @ [Label_out l_out],
    varenv, constenv
  | Skip -> [], varenv, constenv
  | Block s -> compile_statement_pyc s varenv constenv
  | _ -> failwith "Unsupported statement"

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

let compile_pyc s =
  reset ();
  let pyc, varenv, constenv = compile_statement_pyc s [] [] in
  resolve_label pyc

let compile name s =
  reset ();
  let pyc, varenv, constenv = compile_statement_pyc s [] [] in
  let varenv, constenv = List.rev varenv, List.rev constenv in
  let pyc = resolve_label pyc in
  generate_pycode name varenv constenv pyc
