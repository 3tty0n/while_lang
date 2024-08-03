open Syntax

type pyc =
  | UNARY_NOT
  | BINARY_ADD
  | BINARY_SUBTRACT
  | BINARY_MULTIPLY
  | BINARY_TRUE_DIVIDE
  | BINARY_AND
  | BINARY_OR
  | COMPARE_OP of int
  | LOAD_NAME of int            (* opcode, operand *)
  | STORE_NAME of int           (* opcode, operand  *)
  | LOAD_CONST of int
  | SETUP_LOOP of int
  | POP_BLOCK
  | JUMP_ABSOLETE of int
  | JUMP_FORWARD of int
  | POP_JUMP_IF_FALSE of int
  | POP_JUMP_IF_TRUE of int
  | RETURN_VALUE
  | Label of string             (* this opcode will be removed later *)
  | Jump of string              (* label name. will be converted into JUMP_ABSOLETE or other stuff *)
  | Jump_if of string

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
  | COMPARE_OP i -> print_string "COMPARE_OP\t"; print_int i; print_newline ()
  | JUMP_ABSOLETE i -> print_string "JUMP_ABSOLETE\t"; print_int i; print_newline ()
  | JUMP_FORWARD i -> print_string "JUMP_FORWARD\t"; print_int i; print_newline ()
  | POP_JUMP_IF_TRUE i -> print_string "POP_JUMP_IF_TRUE\t"; print_int i; print_newline ()
  | POP_JUMP_IF_FALSE i -> print_string "POP_JUMP_IF_FALSE\t"; print_int i; print_newline ()
  | RETURN_VALUE -> print_string "RETURN_VALUE"; print_newline ()
  | SETUP_LOOP i -> print_string "SETUP_LOOP\t"; print_int i; print_newline ()
  | POP_BLOCK -> print_endline "POP_BLOCK"
  | Jump s -> print_string "Jump\t"; print_endline s
  | Jump_if s -> print_string "Jump_if\t"; print_endline s
  | Label s -> print_string "Label: "; print_endline s

let count_varmap = ref (-1)
let count_constmap = ref (-1)
let count_cmpmap = ref (-1)

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


let create_env_tuple varenv constenv cmpenv =
  let var_tuple = List.map fst varenv in
  let const_tuple = List.map fst constenv in
  var_tuple, const_tuple


let rec compile_pyc s =
  let pyc, varenv, constenv, cmpenv = compile_statement_pyc s [] [] [] in
  List.iter print_pyc pyc;
  pyc

let outpu_byte oc byte =
  Printf.fprintf oc "%a" output_byte byte

let output_bytes oc bytes =
  Printf.fprintf oc "%a" output_bytes bytes


let type_null      = '0'
let type_none      = 'N'
let type_false     = 'F'
let type_true      = 'T'
let type_stopiter  = 'S'
let type_ellipsis  = '.'
let type_int       = 'i'
let type_int64     = 'I'
let type_float     = 'f'
let type_string    = 's'
let type_interned  = 't'
let type_stringref = 'R'
let type_tuple    = '('
let type_list      = '['
let type_dict      = '{'
let type_code      = 'c'
let type_unicode   = 'u'
let type_unknown   = '?'
let type_set       = '<'
let type_frozenset = '>'

(* let marshal_null oc _ = *)
(*   output_byte oc (type_null) *)

let marshal_bool oc b =
  if b then Printf.fprintf oc "%a" output_byte (Char.code type_true)
  else Printf.fprintf oc "%a" outpu_byte (Char.code type_false)

let put_int oc n =
  let a = Char.chr (Int.logand n 0xff) in
  let n = Int.shift_right n 8 in
  let b = Char.chr (Int.logand n 0xff) in
  let n = Int.shift_right n 8 in
  let c = Char.chr (Int.logand n 0xff) in
  let n = Int.shift_right n 8 in
  let d = Char.chr (Int.logand n 0xff) in
  Printf.fprintf oc "%c" a;
  Printf.fprintf oc "%c" b;
  Printf.fprintf oc "%c" c;
  Printf.fprintf oc "%c" d

let atom_int oc n =
  Printf.fprintf oc "%c" type_int;
  put_int oc n

let put_str oc s =
  put_int oc (String.length s);
  Printf.fprintf oc "%s" s

let atom_str oc s =
  Printf.fprintf oc "%c" type_string;
  put_int oc (String.length s);
  Printf.fprintf oc "%s" s

let marshal_int oc n = atom_int oc n

let marshal_str oc s = atom_str oc s

let marshal_tuple oc f lst =
  Printf.fprintf oc "%c" type_tuple;
  put_int oc (List.length lst);
  List.iter (fun x -> f oc x) lst

let marshal_pycode oc pycode =
  Printf.fprintf oc "%c" type_code;
  put_int oc 0;                  (* argcount *)
  put_int oc 0;                  (* nlocals *)
  put_int oc 0;                  (* stacksize *)
  put_int oc 64;                 (* flags *)
  marshal_str oc (Bytes.create 10 |> Bytes.to_string);  (* code *)
  marshal_tuple oc atom_int [1];       (* consts *)
  marshal_tuple oc atom_str ["x"];    (* names *)
  marshal_tuple oc atom_str [];    (* varnames *)
  marshal_tuple oc atom_str [];    (* freevars *)
  marshal_tuple oc atom_str [];    (* cellvars *)
  atom_str oc "test";
  atom_str oc "<module>";
  put_int oc 0;
  marshal_str oc (Bytes.create 1 |> Bytes.to_string)
