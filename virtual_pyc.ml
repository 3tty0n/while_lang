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
  | Label_top of (string * string)             (* this opcode will be removed later *)
  | Label_out of string             (* this opcode will be removed later *)
  | Jump of string              (* label name. will be converted into JUMP_ABSOLETE or other stuff *)
  | Jump_if of string

let has_operand = function
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
  | Label_out _ -> false
  | COMPARE_OP _
  | LOAD_NAME _
  | STORE_NAME _
  | LOAD_CONST _
  | SETUP_LOOP _
  | JUMP_ABSOLETE _
  | JUMP_FORWARD _
  | POP_JUMP_IF_FALSE _
  | POP_JUMP_IF_TRUE _
  | Label_top _
  | Jump _ | Jump_if _ -> true

(* representation of CPython value *)
type w_object =
  | W_Int of int
  | W_String of string
  | W_True
  | W_False
  | W_None

type pycode =
    PyCode of int * int * int * int * bytes * (* argcount, nlocals, stacksize, flags, code *)
              w_object list * w_object list * w_object list * w_object list * w_object list * (* consts, names, varnames, freevars, cellvars, *)
              string * string * int * bytes (* name, filename, firstlineno, lnotab *)

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
  | SETUP_LOOP n  -> [120; n; 0]
  | COMPARE_OP n -> [107; n; 0]
  | LOAD_NAME n -> [101; n; 0]
  | STORE_NAME n -> [90; n; 0]
  | LOAD_CONST n -> [100; n; 0]
  | JUMP_ABSOLETE n -> [113; n; 0]
  | JUMP_FORWARD n -> [110; n; 0]
  | POP_JUMP_IF_FALSE n -> [114; n; 0]
  | POP_JUMP_IF_TRUE n -> [115; n; 0]
  | _ -> failwith ("Unmatched opcode")

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
  | Label_top (s1, s2) -> print_string "Label_top: "; print_endline s1
  | Label_out s -> print_string "Label_out: "; print_endline s

let print_pyc_list pyc =
  let pc = ref 0 in
  List.iter (fun pyc ->
      print_int !pc; print_string "\t"; print_pyc pyc;
      if has_operand pyc then
        pc := !pc + 3
      else incr pc;
    ) pyc

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

let resolve_label pyc =
  let pyc_with_index =
    let pc = ref 0 in
    List.map (fun pyc ->
        let result = (!pc, pyc) in
        if has_operand pyc then (
          pc := !pc + 3
        ) else
          pc := !pc + 1;
        result
      ) pyc
  in
  let label_map = Hashtbl.create 10 in
  let rec aux_label pyc =
    match pyc with
    | [] -> []
    | (i, Label_top (l_top, l_out)) :: tl ->
      Hashtbl.add label_map l_top (i + 3);
      let pc_out = Hashtbl.find label_map l_out in
      [i, SETUP_LOOP (pc_out - (i + 3))] @ aux_label tl
    | (i, Label_out l) :: tl ->
      Hashtbl.add label_map l i;
      [i, POP_BLOCK] @ aux_label tl
    | hd :: tl -> hd :: (aux_label tl)
  in
  let rec aux_jump pyc =
    match pyc with
    | [] -> []
    | (i, Jump l) :: tl ->
      let addr = Hashtbl.find label_map l in
      [JUMP_ABSOLETE addr] @ (aux_jump tl)
    | (i, Jump_if l) :: tl ->
      let addr = Hashtbl.find label_map l in
      [POP_JUMP_IF_FALSE addr] @ (aux_jump tl)
    | hd :: tl -> (snd hd) :: aux_jump tl
  in List.rev (aux_jump (aux_label (List.rev pyc_with_index)))

let compile_pyc s =
  reset ();
  let pyc, _, _ = compile_statement_pyc s [] [] in
  let pyc = resolve_label pyc in
  pyc

let compile_and_create_pycode s =
  reset ();
  let pyc, varenv, constenv = compile_statement_pyc s [] [] in
  let pyc = resolve_label pyc in
  (* for dummy return *)
  let w_none_reg_num = !count_constenv + 1 in
  let const_tuple = List.map fst (constenv @ [(W_None, w_none_reg_num)]) in
  let var_tuple = List.map fst varenv in
  let code = create_bytes_pyc (pyc @ [LOAD_CONST w_none_reg_num; RETURN_VALUE]) in
  let pycode =
    create_pycode 0 4 4 64 code
      const_tuple var_tuple [] [] []
      "test.py" "<module>" 0 (Bytes.of_string "4") in
  pycode
