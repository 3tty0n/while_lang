(* https://www.cs.cmu.edu/~aldrich/courses/15-819O-13sp/resources/while-language.pdf *)

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
  | Seq of s * s
  | If of p * s * s
  | While of p * s
