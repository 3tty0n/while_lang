(* Visualizer module for While Lang compiler transformations *)
(* Helps students understand each transformation step *)

open Syntax
open Virtual_stack
open Pycode

(* ANSI color codes for terminal output *)
let color_reset = "\027[0m"
let color_bold = "\027[1m"
let color_blue = "\027[34m"
let color_green = "\027[32m"
let color_yellow = "\027[33m"
let color_magenta = "\027[35m"
let color_cyan = "\027[36m"

let section_header title =
  Printf.printf "\n%s%s========================================\n" color_bold color_blue;
  Printf.printf "%s\n" title;
  Printf.printf "========================================%s\n\n" color_reset

(* Pretty print arithmetic expressions *)
let rec string_of_arith = function
  | Var x -> x
  | Num n -> string_of_int n
  | Add (a1, a2) -> Printf.sprintf "(%s + %s)" (string_of_arith a1) (string_of_arith a2)
  | Sub (a1, a2) -> Printf.sprintf "(%s - %s)" (string_of_arith a1) (string_of_arith a2)
  | Mul (a1, a2) -> Printf.sprintf "(%s * %s)" (string_of_arith a1) (string_of_arith a2)
  | Div (a1, a2) -> Printf.sprintf "(%s / %s)" (string_of_arith a1) (string_of_arith a2)

(* Pretty print predicates *)
let rec string_of_pred = function
  | True -> "true"
  | False -> "false"
  | Not p -> Printf.sprintf "not(%s)" (string_of_pred p)
  | And (p1, p2) -> Printf.sprintf "(%s and %s)" (string_of_pred p1) (string_of_pred p2)
  | Or (p1, p2) -> Printf.sprintf "(%s or %s)" (string_of_pred p1) (string_of_pred p2)
  | LT (a1, a2) -> Printf.sprintf "(%s < %s)" (string_of_arith a1) (string_of_arith a2)
  | LE (a1, a2) -> Printf.sprintf "(%s <= %s)" (string_of_arith a1) (string_of_arith a2)
  | EQ (a1, a2) -> Printf.sprintf "(%s == %s)" (string_of_arith a1) (string_of_arith a2)
  | GT (a1, a2) -> Printf.sprintf "(%s > %s)" (string_of_arith a1) (string_of_arith a2)
  | GE (a1, a2) -> Printf.sprintf "(%s >= %s)" (string_of_arith a1) (string_of_arith a2)

(* Pretty print statements with indentation *)
let rec string_of_stmt indent stmt =
  let prefix = String.make indent ' ' in
  match stmt with
  | Assign (x, a) ->
      Printf.sprintf "%s%s := %s" prefix x (string_of_arith a)
  | Skip ->
      Printf.sprintf "%sskip" prefix
  | Block s ->
      Printf.sprintf "%sbegin\n%s\n%send"
        prefix
        (string_of_stmt (indent + 2) s)
        prefix
  | Seq (s1, s2) ->
      Printf.sprintf "%s;\n%s"
        (string_of_stmt indent s1)
        (string_of_stmt indent s2)
  | While (p, s) ->
      Printf.sprintf "%swhile %s do\n%s\n%send"
        prefix
        (string_of_pred p)
        (string_of_stmt (indent + 2) s)
        prefix
  | Print a ->
      Printf.sprintf "%sprint %s" prefix (string_of_arith a)
  | If (p, s1, s2) ->
      Printf.sprintf "%sif %s then\n%s\n%selse\n%s"
        prefix
        (string_of_pred p)
        (string_of_stmt (indent + 2) s1)
        prefix
        (string_of_stmt (indent + 2) s2)

(* Pretty print AST *)
let print_ast ast =
  section_header "STEP 1: Abstract Syntax Tree (AST)";
  Printf.printf "%sThe parsed program structure:%s\n\n" color_green color_reset;
  Printf.printf "%s%s%s\n" color_yellow (string_of_stmt 0 ast) color_reset

(* Pretty print stack machine instruction *)
let string_of_stack_instr = function
  | LPush x -> Printf.sprintf "LPUSH %s        # Store stack top to variable '%s'" x x
  | RValue x -> Printf.sprintf "RVALUE %s       # Load value of '%s' to stack" x x
  | Push n -> Printf.sprintf "PUSH %d         # Push constant %d to stack" n n
  | PLUS -> "PLUS           # Pop two values, push their sum"
  | MINUS -> "MINUS          # Pop two values, push their difference"
  | TIMES -> "TIMES          # Pop two values, push their product"
  | DIV -> "DIV            # Pop two values, push their quotient"
  | LabelTest (test, out) -> Printf.sprintf "LABEL_TEST %s (exit: %s)  # Loop condition test point" test out
  | LabelOut (test, out) -> Printf.sprintf "LABEL_OUT %s (test: %s)   # Loop exit point" out test
  | GoTo s -> Printf.sprintf "GOTO %s        # Unconditional jump" s
  | GoFalse s -> Printf.sprintf "GOFALSE %s     # Jump if stack top is false" s
  | NOT -> "NOT            # Logical negation"
  | AND -> "AND            # Logical AND"
  | OR -> "OR             # Logical OR"
  | TRUE -> "TRUE           # Push boolean true"
  | FALSE -> "FALSE          # Push boolean false"
  | EQ -> "EQ             # Equality comparison"
  | LT -> "LT             # Less than comparison"
  | LE -> "LE             # Less than or equal"
  | GT -> "GT             # Greater than comparison"
  | GE -> "GE             # Greater than or equal"
  | PRINT -> "PRINT          # Print stack top value"

(* Pretty print stack machine code *)
let print_stack_code stack_code =
  section_header "STEP 2: Virtual Stack Machine Code";
  Printf.printf "%sStack-based intermediate representation:%s\n\n" color_green color_reset;
  List.iteri (fun i instr ->
    Printf.printf "%s%3d: %s%s%s\n"
      color_cyan i color_yellow (string_of_stack_instr instr) color_reset
  ) stack_code

(* Pretty print Python bytecode instruction *)
let string_of_pyc_instr = function
  | LOAD_NAME i -> Printf.sprintf "LOAD_NAME %d" i
  | STORE_NAME i -> Printf.sprintf "STORE_NAME %d" i
  | LOAD_CONST i -> Printf.sprintf "LOAD_CONST %d" i
  | BINARY_ADD -> "BINARY_ADD"
  | BINARY_SUBTRACT -> "BINARY_SUBTRACT"
  | BINARY_MULTIPLY -> "BINARY_MULTIPLY"
  | BINARY_TRUE_DIVIDE -> "BINARY_TRUE_DIVIDE"
  | BINARY_AND -> "BINARY_AND"
  | BINARY_OR -> "BINARY_OR"
  | UNARY_NOT -> "UNARY_NOT"
  | COMPARE_OP i ->
      let op_name = match i with
        | 0 -> "<"
        | 1 -> "<="
        | 2 -> "=="
        | 4 -> ">"
        | 5 -> ">="
        | _ -> "?"
      in
      Printf.sprintf "COMPARE_OP %d (%s)" i op_name
  | JUMP_ABSOLETE i -> Printf.sprintf "JUMP_ABSOLETE %d" i
  | JUMP_FORWARD i -> Printf.sprintf "JUMP_FORWARD %d" i
  | POP_JUMP_IF_FALSE i -> Printf.sprintf "POP_JUMP_IF_FALSE %d" i
  | POP_JUMP_IF_TRUE i -> Printf.sprintf "POP_JUMP_IF_TRUE %d" i
  | SETUP_LOOP i -> Printf.sprintf "SETUP_LOOP %d" i
  | POP_BLOCK -> "POP_BLOCK"
  | PRINT_ITEM -> "PRINT_ITEM"
  | PRINT_NEWLINE -> "PRINT_NEWLINE"
  | RETURN_VALUE -> "RETURN_VALUE"
  | Label_top (test, out) -> Printf.sprintf "LABEL_TOP %s (exit: %s)" test out
  | Label_out s -> Printf.sprintf "LABEL_OUT %s" s
  | Jump s -> Printf.sprintf "JUMP %s" s
  | Jump_if s -> Printf.sprintf "JUMP_IF %s" s

(* Pretty print Python bytecode *)
let print_python_bytecode bytecode consts names =
  section_header "STEP 3: Python Bytecode";
  Printf.printf "%sPython 2 bytecode instructions:%s\n\n" color_green color_reset;

  (* Print constants table *)
  Printf.printf "%sConstants:%s\n" color_magenta color_reset;
  List.iteri (fun i c ->
    let s = match c with
      | W_Int n -> string_of_int n
      | W_String s -> Printf.sprintf "\"%s\"" s
      | W_True -> "True"
      | W_False -> "False"
      | W_None -> "None"
    in
    Printf.printf "  %d: %s\n" i s
  ) consts;

  Printf.printf "\n%sNames:%s\n" color_magenta color_reset;
  List.iteri (fun i name ->
    match name with
    | W_String s -> Printf.printf "  %d: %s\n" i s
    | _ -> ()
  ) names;

  Printf.printf "\n%sBytecode:%s\n" color_magenta color_reset;
  List.iteri (fun i instr ->
    Printf.printf "%s%3d: %s%s%s\n"
      color_cyan i color_yellow (string_of_pyc_instr instr) color_reset
  ) bytecode

(* Pretty print WebAssembly instruction *)
let string_of_wasm_op = function
  | PLUS -> "i32.add"
  | MINUS -> "i32.sub"
  | TIMES -> "i32.mul"
  | DIV -> "i32.div_s"
  | EQ -> "i32.eq"
  | LT -> "i32.lt_s"
  | LE -> "i32.le_s"
  | GT -> "i32.gt_s"
  | GE -> "i32.ge_s"
  | AND -> "i32.and"
  | OR -> "i32.or"
  | _ -> "???"

let print_wasm_overview stack_code =
  section_header "STEP 3: WebAssembly Code Generation";
  Printf.printf "%sWebAssembly text format (.wat):%s\n\n" color_green color_reset;

  (* Collect globals *)
  let globals = Hashtbl.create 10 in
  let rec collect = function
    | [] -> ()
    | LPush x :: rest | RValue x :: rest ->
        Hashtbl.replace globals x ();
        collect rest
    | _ :: rest -> collect rest
  in
  collect stack_code;

  Printf.printf "%sGlobal Variables:%s\n" color_magenta color_reset;
  Hashtbl.iter (fun var _ ->
    Printf.printf "  (global $%s (mut i32) (i32.const 0))\n" var
  ) globals;

  Printf.printf "\n%sKey Operations:%s\n" color_magenta color_reset;
  Printf.printf "  • Variable loads: global.get $var\n";
  Printf.printf "  • Variable stores: global.set $var\n";
  Printf.printf "  • Arithmetic: %s\n"
    (String.concat ", " ["i32.add"; "i32.sub"; "i32.mul"; "i32.div_s"]);
  Printf.printf "  • Control flow: loop, block, br, br_if\n";
  Printf.printf "  • I/O: call $print (imported function)\n"

(* Main visualization function *)
let visualize_transformations filename ast stack_code output_format =
  Printf.printf "\n";
  Printf.printf "%s%s╔════════════════════════════════════════════════════════════╗%s\n"
    color_bold color_blue color_reset;
  Printf.printf "%s%s║  While Lang Compiler: Transformation Visualization        ║%s\n"
    color_bold color_blue color_reset;
  Printf.printf "%s%s╚════════════════════════════════════════════════════════════╝%s\n"
    color_bold color_blue color_reset;
  Printf.printf "\n%sInput file:%s %s\n" color_green color_reset filename;

  (* Step 1: Show AST *)
  print_ast ast;

  (* Step 2: Show Stack Machine Code *)
  print_stack_code stack_code;

  (* Step 3: Show final output based on format *)
  match output_format with
  | "wasm" -> print_wasm_overview stack_code
  | "pyc" ->
      (* Compile to Python bytecode *)
      let (bytecode, varenv, constenv) = Emit_pyc.compile_stack_pyc [] [] [] stack_code in
      let resolved = Pycode.resolve_label bytecode in
      (* Extract names and constants from environments *)
      let names = List.map fst varenv in
      let consts = List.map fst constenv in
      print_python_bytecode resolved consts names
  | _ -> ()

(* Summary comparison *)
let print_summary () =
  Printf.printf "\n";
  section_header "Transformation Summary";
  Printf.printf "%sSource Code →%s\n" color_green color_reset;
  Printf.printf "  Parse and build AST\n";
  Printf.printf "%s↓%s\n" color_blue color_reset;
  Printf.printf "%sAbstract Syntax Tree →%s\n" color_green color_reset;
  Printf.printf "  Compile to stack-based IR\n";
  Printf.printf "%s↓%s\n" color_blue color_reset;
  Printf.printf "%sStack Machine Code →%s\n" color_green color_reset;
  Printf.printf "  Generate target code\n";
  Printf.printf "%s↓%s\n" color_blue color_reset;
  Printf.printf "%sWebAssembly or Python Bytecode%s\n\n" color_green color_reset
