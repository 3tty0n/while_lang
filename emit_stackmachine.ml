open Syntax

type t =
  | LValue of string
  | RValue of string
  | Push of int
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | Label of string
  | GoTo of string
  | GoFalse of string
  | True
  | False
  | EQ
  | LT
  | LE
  | GT
  | GE

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

let compile_predicate (predicate : p) : t list =
  match predicate with
  | True -> [True]
  | False -> [False]
  | EQ (a1, a2) -> (compile_arith a1) @ (compile_arith a2) @ [EQ]
  | LT (a1, a2) -> (compile_arith a1) @ (compile_arith a2) @ [LT]
  | LE (a1, a2) -> (compile_arith a1) @ (compile_arith a2) @ [LE]
  | GT (a1, a2) -> (compile_arith a1) @ (compile_arith a2) @ [GT]
  | GE (a1, a2) -> (compile_arith a1) @ (compile_arith a2) @ [GE]

let rec compile_statement (statement : s) : t list =
  match statement with
  | Assign (id, arith) ->
    LValue (id) :: (compile_arith arith)
  | Skip -> []
  | Block (stmt) ->
    compile_statement stmt
  | Seq (stmt1, stmt2) ->
    (compile_statement stmt1) @ (compile_statement stmt2)
  | While (pred, stmt) ->
    [Label ("test")] @ (compile_predicate pred) @
    [GoFalse ("out")] @ (compile_statement stmt) @ [GoTo ("test")] @
    [Label ("out")]


let print_t oc t =
  match t with
  | LValue (id) -> Printf.fprintf oc "lvalue\t%s\n" id
  | RValue (id) -> Printf.fprintf oc "rvalue\t%s\n" id
  | Push (n) -> Printf.fprintf oc "push\t%d\n" n
  | PLUS -> Printf.fprintf oc "+\n"
  | MINUS -> Printf.fprintf oc "-\n"
  | TIMES -> Printf.fprintf oc "*\n"
  | DIV -> Printf.fprintf oc "/\n"
  | LT -> Printf.fprintf oc "<\n"
  | Label (l) -> Printf.fprintf oc "label\t%s\n" l
  | GoFalse (l) -> Printf.fprintf oc "gofalse\t%s\n" l
  | GoTo (l) -> Printf.fprintf oc "goto\t%s\n" l

let rec print_code oc code =
  match code with
  | [] -> []
  | hd :: tl -> print_t oc hd; print_code oc tl
