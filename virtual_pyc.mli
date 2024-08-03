type w_object =
  | W_Int of int
  | W_String of string
  | W_True
  | W_False
  | W_None

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
  | Label_top of (string * string)      (* this opcode will be removed later *)
  | Label_out of string                 (* this opcode will be removed later *)
  | Jump of string              (* label name. will be converted into JUMP_ABSOLETE or other stuff *)
  | Jump_if of string

type p = {
  argcount: int; nlocals: int; stacksize: int; flags: int;
  code: bytes;
  consts: w_object list; names: w_object list;
  varnames: w_object list; freevars: w_object list; cellvars: w_object list;
  name: string; filename: string; firstlineno: int; lnotab: bytes
}

type pycode = PyCode of p

val print_pyc : pyc -> unit
val print_pyc_list : pyc list -> unit
val compile_pyc : Syntax.s -> pyc list
val compile : string -> Syntax.s -> pycode
