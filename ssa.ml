open Syntax

module M = Map.Make(String)

let m : (string * int) list ref = ref []

let id_num = ref (-1)

let gen_id_num () =
  incr id_num; !id_num

let rec assign_id_arith (arith : Syntax.a) =
  match arith with
  | Num n -> Num n
  | Var (id) ->
    (match List.assoc_opt id !m with
     | Some id_num -> Var (id ^ (string_of_int id_num))
     | None ->
       let id_num = gen_id_num () in
       m := (id, id_num) :: !m;
       Var (id ^ (string_of_int id_num)))
  | Add (lhs, rhs) ->
    Add (assign_id_arith lhs, assign_id_arith rhs)
  | Sub (lhs, rhs) ->
    Sub (assign_id_arith lhs, assign_id_arith rhs)
  | Mul (lhs, rhs) ->
    Mul (assign_id_arith lhs, assign_id_arith rhs)
  | Div (lhs, rhs) ->
    Div (assign_id_arith lhs, assign_id_arith rhs)


let rec assign_id_pred (pred : Syntax.p) =
  match pred with
  | True | False | Not _ | And _ | Or _ -> pred
  | LT (a1, a2) -> LT (assign_id_arith a1, assign_id_arith a2)
  | LE (a1, a2) -> LE (assign_id_arith a1, assign_id_arith a2)
  | GT (a1, a2) -> GT (assign_id_arith a1, assign_id_arith a2)
  | GE (a1, a2) -> GE (assign_id_arith a1, assign_id_arith a2)
  | EQ (a1, a2) -> EQ (assign_id_arith a1, assign_id_arith a2)


let rec assign_id_statement (stmt : Syntax.s) =
  match stmt with
  | Assign (id, a) ->
    let a = assign_id_arith a in
    let id_num = gen_id_num () in
    m := (id, id_num) :: !m;
    Assign (id ^ (string_of_int id_num), a)
  | Skip -> Skip
  | Block (s) -> Block (assign_id_statement s)
  | Seq (s1, s2) ->
    let s1 = assign_id_statement s1 in
    let s2 = assign_id_statement s2 in
    Seq (s1, s2)
  | While (p, s) ->
    While (assign_id_pred p, assign_id_statement s)
