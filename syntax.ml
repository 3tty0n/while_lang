(* https://www.cs.cmu.edu/~aldrich/courses/15-819O-13sp/resources/ *)

open Printf

type id = string
type num = int

(* arithmetic expressions *)
type a =
  | Var of id
  | Num of num
  | Add of a * a
  | Sub of a * a
  | Mul of a * a
  | Div of a * a

(* boolean predicates *)
type p =
    True
  | False
  | Not of p
  | And of p * p
  | Or of p * p
  | LT of a * a
  | LE of a * a
  | EQ of a * a
  | GT of a * a
  | GE of a * a

(* statements *)
type s =
  | Assign of id * a
  | Skip
  | Block of s
  | Seq of s * s
  | If of p * s * s
  | While of p * s

type ss =
  | Statement of s
  | Seq of s * ss

let rec string_of_arith a =
  match a with
  | Var id -> sprintf "Var(%s)" id
  | Num n -> sprintf "Num(%d)" n
  | Add (a1, a2) -> sprintf "Add (%s, %s)" (string_of_arith a1) (string_of_arith a2)
  | Sub (a1, a2) -> sprintf "Sub (%s, %s)" (string_of_arith a1) (string_of_arith a2)
  | Mul (a1, a2) -> sprintf "Mul (%s, %s)" (string_of_arith a1) (string_of_arith a2)
  | Div (a1, a2) -> sprintf "Div (%s, %s)" (string_of_arith a1) (string_of_arith a2)

let rec string_of_predicate p =
  match p with
  | True -> "true"
  | False -> "false"
  | Not p -> sprintf "not %s" (string_of_predicate p)
  | And (p1, p2) -> sprintf "%s and %s" (string_of_predicate p1) (string_of_predicate p2)
  | Or (p1, p2) -> sprintf "%s or %s" (string_of_predicate p1) (string_of_predicate p2)
  | LT (a1, a2) -> sprintf "%s < %s" (string_of_arith a1) (string_of_arith a2)
  | LE (a1, a2) -> sprintf "%s <= %s" (string_of_arith a1) (string_of_arith a2)
  | GT (a1, a2) -> sprintf "%s > %s" (string_of_arith a1) (string_of_arith a2)
  | GE (a1, a2) -> sprintf "%s >= %s" (string_of_arith a1) (string_of_arith a2)
  | EQ (a1, a2) -> sprintf "%s == %s" (string_of_arith a1) (string_of_arith a2)

let rec string_of_statement s =
  match s with
  | Assign (id, a) -> sprintf "Assing (%s, %s);\n" id (string_of_arith a)
  | Skip -> sprintf "Skip;\n"
  | Block (s) -> sprintf "Block (%s);\n" (string_of_statement s)
  | Seq (s1, s2) -> sprintf "%s%s\n" (string_of_statement s1) (string_of_statement s2)
  | While (p, s) -> sprintf "While (%s, %s);\n" (string_of_predicate p) (string_of_statement s)
