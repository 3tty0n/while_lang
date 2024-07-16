open Syntax

type t =
  | LValue of string
  | RValue of string
  | Push of int
  | PLUS
  | MINUS
  | TIMES
  | DIV

let print_t t =
  match t with
  | LValue (id) -> print_string "lvalue "; print_string id; print_newline ()
  | RValue (id) -> print_string "rvalue "; print_string id; print_newline ()
  | Push (n) -> print_string "push "; print_int n; print_newline ()
  | PLUS -> print_string "+"; print_newline ()
  | MINUS -> print_string "-"; print_newline ()
  | TIMES -> print_string "*"; print_newline ()
  | DIV -> print_string "/"; print_newline ()

let rec print_code code =
  match code with
  | [] -> []
  | hd :: tl -> print_t hd; print_code tl

let rec compile_arith arith =
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

let rec compile_statement statement =
  match statement with
  | Assign (id, arith) ->
    LValue (id) :: (compile_arith arith)
  | Skip -> []
  | Seq (stmt1, stmt2) ->
    (compile_statement stmt1) @ (compile_statement stmt2)
