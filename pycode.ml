type pyc =
  | UNARY_NOT                   (* no oprand *)
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
  | COMPARE_OP of int           (* with operand *)
  | LOAD_NAME of int
  | STORE_NAME of int
  | LOAD_CONST of int
  | SETUP_LOOP of int
  | JUMP_ABSOLETE of int
  | JUMP_FORWARD of int
  | POP_JUMP_IF_FALSE of int
  | POP_JUMP_IF_TRUE of int
  | Label_top of (string * string)  (* this opcode will be removed later *)
  | Label_out of string             (* this opcode will be removed later *)
  | Jump of string                  (* label name. will be converted into JUMP_ABSOLETE or other stuff *)
  | Jump_if of string               (* label name. will be converted into POP_JUMP_IF_FALSE or other stuff *)

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

let string_of_w_object = function
  | W_Int i -> Printf.sprintf "W_Int(%d)" i
  | W_String s -> Printf.sprintf "W_String(%s)" s
  | W_True -> Printf.sprintf "W_True"
  | W_False -> Printf.sprintf "W_False"
  | W_None -> Printf.sprintf "W_None"

type p = {
  argcount: int; nlocals: int; stacksize: int; flags: int;
  code: bytes;
  consts: w_object list; names: w_object list;
  varnames: w_object list; freevars: w_object list; cellvars: w_object list;
  name: string; filename: string; firstlineno: int; lnotab: bytes
}

type pycode = PyCode of p

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
