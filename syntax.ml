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
  | LT of p * p
  | LE of p * p
  | EQ of p * p
  | GT of p * p
  | GE of p * p

(* statements *)
type s =
  | Assign of id * a
  | Skip
  | Seq of s * s
  | If of p * s * s
  | While of p * s
