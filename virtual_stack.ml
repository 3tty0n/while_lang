open Syntax

(* 仮想スタックマシンの命令の宣言 *)
type t =
  | LPush of string
  | RValue of string
  | Push of int
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LabelTest of string * string (* test, out *)
  | LabelOut of string * string  (* test, out *)
  | GoTo of string
  | GoFalse of string
  | NOT
  | AND
  | OR
  | TRUE
  | FALSE
  | EQ
  | LT
  | LE
  | GT
  | GE
  | PRINT

(* While言語の変数、数値や演算を仮想スタックマシンの命令へ変換する *)
let rec compile_arith (arith : a) : t list =
  match arith with
  | Var id -> [RValue (id)]
  | Num n -> [Push n]
  | Add (lhs, rhs) ->
    (compile_arith lhs) @ (compile_arith rhs) @ [PLUS]
  | Sub (lhs, rhs) ->
    (compile_arith lhs) @ (compile_arith rhs) @ [MINUS]
  | Div (lhs, rhs) ->
    (compile_arith lhs) @ (compile_arith rhs) @ [DIV]
  | Mul (lhs, rhs) ->
    (compile_arith lhs) @ (compile_arith rhs) @ [TIMES]

(* While言語の条件式を仮想スタックマシンの命令へ変換する *)
let rec compile_predicate (predicate : p) : t list =
  match predicate with
  | True -> [TRUE]
  | False -> [FALSE]
  | Not (p) -> (compile_predicate p) @ [NOT]
  | And (p1, p2) -> (compile_predicate p1) @ (compile_predicate p2) @ [AND]
  | Or (p1, p2) -> (compile_predicate p1) @ (compile_predicate p2) @ [OR]
  | EQ (a1, a2) -> (compile_arith a1) @ (compile_arith a2) @ [OR]
  | LT (a1, a2) -> (compile_arith a1) @ (compile_arith a2) @ [LT]
  | LE (a1, a2) -> (compile_arith a1) @ (compile_arith a2) @ [LE]
  | GT (a1, a2) -> (compile_arith a1) @ (compile_arith a2) @ [GT]
  | GE (a1, a2) -> (compile_arith a1) @ (compile_arith a2) @ [GE]

let count = ref (-1)
let gen_label () =
  count := !count + 1;
  "L." ^ (string_of_int !count)

let reset () =
  count := (-1)

(* While言語の文を仮想スタックマシンの命令へ変換する *)
let rec compile_statement (statement : s) : t list =
  match statement with
  | Assign (id, arith) ->
    (compile_arith arith) @ [LPush (id)]
  | Skip -> []
  | Block (stmt) ->
    compile_statement stmt
  | Seq (stmt1, stmt2) ->
    (compile_statement stmt1) @ (compile_statement stmt2)
  | While (pred, stmt) ->
    let test = gen_label () in
    let out = gen_label () in
    [LabelTest (test, out)] @ (compile_predicate pred) @
    [GoFalse (out)] @ (compile_statement stmt) @ [GoTo (test)] @
    [LabelOut (test, out)]
  | Print (arith) ->
    (compile_arith arith) @ [PRINT]
  | _ -> failwith "Unsupported statement"


let compile_stack statement =
  reset ();
  compile_statement statement


let print_t oc t =
  match t with
  | LPush (id) -> Printf.fprintf oc "lpush\t%s\n" id
  | RValue (id) -> Printf.fprintf oc "rvalue\t%s\n" id
  | Push (n) -> Printf.fprintf oc "push\t%d\n" n
  | PLUS -> Printf.fprintf oc "+\n"
  | MINUS -> Printf.fprintf oc "-\n"
  | TIMES -> Printf.fprintf oc "*\n"
  | DIV -> Printf.fprintf oc "/\n"
  | LT -> Printf.fprintf oc "<\n"
  | LE -> Printf.fprintf oc "<=\n"
  | GT -> Printf.fprintf oc ">\n"
  | GE -> Printf.fprintf oc ">=\n"
  | EQ -> Printf.fprintf oc ">=\n"
  | NOT -> Printf.fprintf oc "not\n"
  | AND -> Printf.fprintf oc "and\n"
  | OR -> Printf.fprintf oc "or\n"
  | TRUE-> Printf.fprintf oc "true\n"
  | FALSE-> Printf.fprintf oc "false\n"
  | LabelTest (l, m) -> Printf.fprintf oc "label\t%s\n" l
  | LabelOut (l, m) -> Printf.fprintf oc "label\t%s\n" m
  | GoFalse (l) -> Printf.fprintf oc "gofalse\t%s\n" l
  | GoTo (l) -> Printf.fprintf oc "goto\t%s\n" l
  | PRINT -> Printf.fprintf oc "print\n"

let rec print_code oc code =
  match code with
  | [] -> ()
  | hd :: tl -> print_t oc hd; print_code oc tl
