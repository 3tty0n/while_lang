open Syntax

type pyc =
  | UNARY_NOT
  | BINARY_ADD
  | BINARY_SUBTRACT
  | BINARY_MULTIPLY
  | BINARY_TRUE_DIVIDE
  | BINARY_AND
  | BINARY_OR
  | POP_BLOCK
  | RETURN_VALUE
  | PRINT_ITEM
  | PRINT_NEWLINE
  | COMPARE_OP of int
  | LOAD_NAME of int            (* opcode, operand *)
  | STORE_NAME of int           (* opcode, operand  *)
  | LOAD_CONST of int
  | SETUP_LOOP of int
  | JUMP_ABSOLETE of int
  | JUMP_FORWARD of int
  | POP_JUMP_IF_FALSE of int
  | POP_JUMP_IF_TRUE of int
  | Label of string             (* this opcode will be removed later *)
  | Jump of string              (* label name. will be converted into JUMP_ABSOLETE or other stuff *)
  | Jump_if of string

let ascii_of_opcode opcode =
  match opcode with
  | UNARY_NOT -> [12]
  | BINARY_ADD -> [23]
  | BINARY_SUBTRACT -> [24]
  | BINARY_MULTIPLY -> [20]
  | BINARY_TRUE_DIVIDE -> [29]
  | BINARY_AND -> [64]
  | BINARY_OR -> [66]
  | PRINT_ITEM -> [71]
  | PRINT_NEWLINE -> [72]
  | RETURN_VALUE -> [83]
  | POP_BLOCK -> [87]
  | COMPARE_OP n -> [107; n; 0]
  | LOAD_NAME n -> [101; n; 0]
  | STORE_NAME n -> [90; n; 0]
  | LOAD_CONST n -> [100; n; 0]
  | JUMP_ABSOLETE n -> [113; n; 0]
  | JUMP_FORWARD n -> [110; n; 0]
  | POP_JUMP_IF_FALSE n -> [114; n; 0]
  | POP_JUMP_IF_TRUE n -> [115; n; 0]
  | _ -> failwith "Unmatched opcode"

let rec opcode_list_of_pyc pyc =
  match pyc with
  | [] -> []
  | hd :: tl -> (ascii_of_opcode hd) @ opcode_list_of_pyc tl

let create_bytes_pyc pyc =
  let opcode_list = opcode_list_of_pyc pyc in
  let char_list = List.map char_of_int opcode_list in
  let bytes = Bytes.create (List.length char_list) in
  Bytes.iteri (fun i c -> Bytes.set bytes i (List.nth char_list i)) bytes;
  bytes

type pycode =
   (* argcount, nlocals, stacksize, flags, code, consts, names, varnames, freevars, cellvars, filename, name, firstlineno, lnotab *)
    PyCode of int * int * int * int * bytes * int list * string list * string list * string list * string list * string * string * int * bytes

let get_code = function
    PyCode (_, _, _, _, code, _, _,  _, _, _, _, _, _, _) ->
    code

let create_pycode argcount nlocals stacksize flags code consts names varnames freevars cellvars filename name firstlineno lnotab =
  PyCode (argcount, nlocals, stacksize, flags, code, consts, names, varnames, freevars, cellvars, filename, name, firstlineno, lnotab)

let print_pyc = function
  | LOAD_NAME i -> print_string "LOAD_NAME\t"; print_int i; print_newline ()
  | STORE_NAME i -> print_string "STORE_NAME\t"; print_int i; print_newline ()
  | LOAD_CONST i  -> print_string "LOAD_CONST\t"; print_int i; print_newline ()
  | BINARY_ADD -> print_endline "BINARY_ADD"
  | BINARY_SUBTRACT -> print_endline "BINARY_SUBTRACT"
  | BINARY_MULTIPLY -> print_endline "BINARY_MULTIPLY"
  | BINARY_AND -> print_endline "BINARY_AND"
  | BINARY_OR -> print_endline "BINARY_OR"
  | BINARY_TRUE_DIVIDE -> print_endline "BINARY_TRUE_DIVIDE"
  | UNARY_NOT ->  print_endline "UNARY_NOT"
  | POP_BLOCK -> print_endline "POP_BLOCK"
  | PRINT_ITEM -> print_endline "PRINT_ITEM"
  | PRINT_NEWLINE -> print_endline "PRINT_NEWLINE"
  | COMPARE_OP i -> print_string "COMPARE_OP\t"; print_int i; print_newline ()
  | JUMP_ABSOLETE i -> print_string "JUMP_ABSOLETE\t"; print_int i; print_newline ()
  | JUMP_FORWARD i -> print_string "JUMP_FORWARD\t"; print_int i; print_newline ()
  | POP_JUMP_IF_TRUE i -> print_string "POP_JUMP_IF_TRUE\t"; print_int i; print_newline ()
  | POP_JUMP_IF_FALSE i -> print_string "POP_JUMP_IF_FALSE\t"; print_int i; print_newline ()
  | RETURN_VALUE -> print_string "RETURN_VALUE"; print_newline ()
  | SETUP_LOOP i -> print_string "SETUP_LOOP\t"; print_int i; print_newline ()
  | Jump s -> print_string "Jump\t"; print_endline s
  | Jump_if s -> print_string "Jump_if\t"; print_endline s
  | Label s -> print_string "Label: "; print_endline s

let count_varmap = ref (-1)
let count_constmap = ref (-1)
let count_cmpmap = ref (-1)

let reset _ =
  count_varmap := (-1);
  count_constmap := (-1);
  count_cmpmap := (-1)

let find_varmap id varmap =
  try List.assoc id varmap, varmap
  with Not_found ->
    incr count_varmap;
    let varmap = (id, !count_varmap) :: varmap in
    !count_varmap, varmap

let find_constmap n constmap =
  try List.assoc n constmap, constmap
  with Not_found ->
    incr count_constmap;
    let constmap =  (n, !count_constmap) :: constmap in
    !count_constmap, constmap

let find_cmpmap n cmpmap =
  try List.assoc n cmpmap, cmpmap
  with Not_found ->
    incr count_cmpmap;
    let cmpmap =  (n, !count_cmpmap) :: cmpmap in
    !count_cmpmap, cmpmap

let rec compile_arith_pyc a varenv constenv cmpenv =
  match a with
  | Var id ->
    let reg_num, varenv = find_varmap id varenv  in
    [LOAD_NAME reg_num], varenv, constenv, cmpenv
  | Num n ->
    let reg_num, constenv = find_constmap n constenv in
    [LOAD_CONST reg_num], varenv, constenv, cmpenv
  | Add (lhs, rhs) ->
    let pyc1, varenv, constenv, cmpenv = compile_arith_pyc lhs varenv constenv cmpenv in
    let pyc2, varenv, constenv, cmpenv = compile_arith_pyc rhs varenv constenv cmpenv in
    pyc1 @ pyc2 @ [BINARY_ADD], varenv, constenv, cmpenv
  | Sub (lhs, rhs) ->
    let pyc1, varenv, constenv, cmpenv = compile_arith_pyc lhs varenv constenv cmpenv in
    let pyc2, varenv, constenv, cmpenv = compile_arith_pyc rhs varenv constenv cmpenv in
    pyc1 @ pyc2 @ [BINARY_ADD], varenv, constenv, cmpenv
  | Mul (lhs, rhs) ->
    let pyc1, varenv, constenv, cmpenv = compile_arith_pyc lhs varenv constenv cmpenv in
    let pyc2, varenv, constenv, cmpenv = compile_arith_pyc rhs varenv constenv cmpenv in
    pyc1 @ pyc2 @ [BINARY_MULTIPLY], varenv, constenv, cmpenv
  | Div (lhs, rhs) ->
    let pyc1, varenv, constenv, cmpenv = compile_arith_pyc lhs varenv constenv cmpenv in
    let pyc2, varenv, constenv, cmpenv = compile_arith_pyc rhs varenv constenv cmpenv in
    pyc1 @ pyc2 @ [BINARY_TRUE_DIVIDE], varenv, constenv, cmpenv


let rec compile_pred_pyc p varenv constenv cmpenv =
  match p with
  | True ->
    let reg_num, varenv = find_varmap "True" varenv in
    [LOAD_NAME reg_num], varenv, constenv, cmpenv
  | False ->
    let reg_num, varenv = find_varmap "False" varenv in
    [LOAD_NAME reg_num], varenv, constenv, cmpenv
  | Not p ->
    let pyc, varenv, constenv, cmpenv = compile_pred_pyc p varenv constenv cmpenv in
    pyc @ [UNARY_NOT], varenv, constenv, cmpenv
  | And (p1, p2) ->
    let py1, varenv, constenv, cmpenv = compile_pred_pyc p1 varenv constenv cmpenv in
    let py2, varenv, constenv, cmpenv = compile_pred_pyc p2 varenv constenv cmpenv in
    py1 @ py2 @ [BINARY_AND], varenv, constenv, cmpenv
  | Or (p1, p2) ->
    let py1, varenv, constenv, cmpenv = compile_pred_pyc p1 varenv constenv cmpenv in
    let py2, varenv, constenv, cmpenv = compile_pred_pyc p2 varenv constenv cmpenv in
    py1 @ py2 @ [BINARY_OR], varenv, constenv, cmpenv
  | LT (a1, a2) ->
    let py1, varenv, constenv, cmpenv = compile_arith_pyc a1 varenv constenv cmpenv in
    let py2, varenv, constenv, cmpenv = compile_arith_pyc a2 varenv constenv cmpenv in
    let reg_num, cmpenv = find_cmpmap "<" cmpenv in
    py1 @ py2 @ [COMPARE_OP reg_num], varenv, constenv, cmpenv
  | LE (a1, a2) ->
    let py1, varenv, constenv, cmpenv = compile_arith_pyc a1 varenv constenv cmpenv in
    let py2, varenv, constenv, cmpenv = compile_arith_pyc a2 varenv constenv cmpenv in
    let reg_num, cmpenv = find_cmpmap "<=" cmpenv in
    py1 @ py2 @ [COMPARE_OP reg_num], varenv, constenv, cmpenv
  | GT (a1, a2) ->
    let py1, varenv, constenv, cmpenv = compile_arith_pyc a1 varenv constenv cmpenv in
    let py2, varenv, constenv, cmpenv = compile_arith_pyc a2 varenv constenv cmpenv in
    let reg_num, cmpenv = find_cmpmap ">" cmpenv in
    py1 @ py2 @ [COMPARE_OP reg_num], varenv, constenv, cmpenv
  | GE (a1, a2) ->
    let py1, varenv, constenv, cmpenv = compile_arith_pyc a1 varenv constenv cmpenv in
    let py2, varenv, constenv, cmpenv = compile_arith_pyc a2 varenv constenv cmpenv in
    let reg_num, cmpenv = find_cmpmap ">=" cmpenv in
    py1 @ py2 @ [COMPARE_OP reg_num], varenv, constenv, cmpenv
  | EQ (a1, a2) ->
    let py1, varenv, constenv, cmpenv = compile_arith_pyc a1 varenv constenv cmpenv in
    let py2, varenv, constenv, cmpenv = compile_arith_pyc a2 varenv constenv cmpenv in
    let reg_num, cmpenv = find_cmpmap "==" cmpenv in
    py1 @ py2 @ [COMPARE_OP reg_num], varenv, constenv, cmpenv


let rec compile_statement_pyc s varenv constenv cmpenv =
  match s with
  | Print (a) ->
    let pyc, varenv, constenv, cmpenv = compile_arith_pyc a varenv constenv cmpenv in
    pyc @ [PRINT_ITEM; PRINT_NEWLINE], varenv, constenv, cmpenv
  | Assign (id, a) ->
    let pyc, varenv, constenv, cmpenv = compile_arith_pyc a varenv constenv cmpenv in
    let reg_num, varenv = find_varmap id varenv in
    pyc @ [STORE_NAME reg_num], varenv, constenv, cmpenv
  | Seq (s1, s2) ->
    let pyc1, varenv, constenv, cmpenv = compile_statement_pyc s1 varenv constenv cmpenv in
    let pyc2, varenv, constenv, cmpenv = compile_statement_pyc s2 varenv constenv cmpenv in
    pyc1 @ pyc2, varenv, constenv, cmpenv
  | While (p, s) ->
    let pyc1, varenv, constenv, cmpenv = compile_pred_pyc p varenv constenv cmpenv in
    let pyc2, varenv, constenv, cmpenv = compile_statement_pyc s varenv constenv cmpenv in
    [Label "l"] @ pyc1 @ [Jump_if "test"] @ pyc2 @ [Jump "l"] @ [Label "test"],
    varenv, constenv, cmpenv

let compile_pyc s =
  reset ();
  let pyc, varenv, constenv, cmpenv = compile_statement_pyc s [] [] [] in
  pyc

let create_env_tuple pyc varenv constenv cmpenv =
  let var_tuple = List.map fst varenv in
  (* for dummy return *)
  let const_tuple = List.map fst constenv in
  let cmp_tuple = List.map fst cmpenv in
  pyc @ [LOAD_CONST 0; RETURN_VALUE], var_tuple, const_tuple, cmp_tuple

let compile_and_create_pycode s =
  reset ();
  let pyc, varenv, constenv, cmpenv = compile_statement_pyc s [] [] [] in
  let pyc, var, const, cmp = create_env_tuple pyc varenv constenv cmpenv in
  let code = create_bytes_pyc pyc in
  let pycode = create_pycode 0 0 0 64 code const var cmp [] [] "test.py" "<module>" 0 (Bytes.of_string "4") in
  pycode
