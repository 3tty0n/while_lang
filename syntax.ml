(* https://www.cs.cmu.edu/~aldrich/courses/15-819O-13sp/resources/ *)
(* Whie言語の文法を定義する *)
(* 具体的には、 type 宣言を使ってデータ型を定義する *)

open Printf

type id = string
type num = int

(* arithmetic expressions *)
type a =
  | Var of id                   (* Var 型。引数は文字列 id *)
  | Num of num                  (* Num 型。引数は数値 num *)
  | Add of a * a                (* Add 型。引数はタプル (arith, arith)。*)
                                (* 再帰的な定義も可能 *)
  | Sub of a * a
  | Mul of a * a
  | Div of a * a

(* boolean predicates *)
type p =
    True                        (* True 型。引数はなし。 *)
  | False
  | Not of p
  | And of p * p
  | Or of p * p                 (* Or 型。 (x | y) に相当。 *)
  | LT of a * a                 (* LT 型。 (x < y) に相当。 *)
  | LE of a * a                 (* LE 型。 (x <= y) に相当。 *)
  | EQ of a * a                 (* EQ 型。 (x == y) に相当。 *)
  | GT of a * a                 (* GT 型。 (x > y) に相当。 *)
  | GE of a * a                 (* GT 型。 (x >= y) に相当。 *)

(* statements *)
type s =
  | Assign of id * a           (* Assign 型。 i := 1; や x := x + y; に相当。 *)
  | Skip
  | Block of s                 (* Block 型。 begin i := 1; end に相当。 *)
  | Seq of s * s               (* Seq 型。連続する文 (i := i + 1; j := i + 1;) に相当。*)
  | While of p * s             (* While 型。 while i < 3 do ... end; に相当 *)
  | Print of a                 (* Print 型。 print 1 に相当 *)
  | If of p * s * s            (* If 型。条件分岐を表わす。 *)

(* デバッグ用の補助関数。 Syntax を文字列として表示する *)
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
  | If (p, s1, s2) -> sprintf "If (%s, %s, %s);\n" (string_of_predicate p) (string_of_statement s1) (string_of_statement s2)
  | Print (a) -> sprintf "Print (%s);\n" (string_of_arith a)
