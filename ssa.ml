(* SSA 変換 (α変換) を行い、全ての変数にユニークなIDを付与する *)

open Syntax

(* 変数名からユニークなIDへの写像を宣言 *)
module M = Map.Make(String)

let id_num = ref (-1)

(* 変数名からユニークな ID を生成する *)
let gen_id s =
  incr id_num;
  s ^ "." ^ (string_of_int !id_num)

(* 写像 M から、変数 x に対応するユニーク ID を取得する *)
(* ユニークな ID が存在しなければ新たな ID を生成する   *)
let find_or_gen_id (x : id) (env : id M.t) =
  try
    M.find x env, env
  with Not_found ->
    let id' = gen_id x in
    let env = M.add x id' env in
    id', env


let rec assign_id_arith (arith : Syntax.a) (env : id M.t) =
  match arith with
  | Num n -> Num n, env
  | Var (id) ->
    let id', env = find_or_gen_id id env in
    Var (id'), env
  | Add (lhs, rhs) ->
    let lhs, env = assign_id_arith lhs env in
    let rhs, env = assign_id_arith rhs env  in
    Add (lhs, rhs), env
  | Sub (lhs, rhs) ->
    let lhs, env = assign_id_arith lhs env in
    let rhs, env = assign_id_arith rhs env  in
    Sub (lhs, rhs), env
  | Mul (lhs, rhs) ->
    let lhs, env = assign_id_arith lhs env in
    let rhs, env = assign_id_arith rhs env  in
    Mul (lhs, rhs), env
  | Div (lhs, rhs) ->
    let lhs, env = assign_id_arith lhs env in
    let rhs, env = assign_id_arith rhs env  in
    Div (lhs, rhs), env


let rec assign_id_pred (pred : Syntax.p) (env : id M.t) =
  match pred with
  | True | False -> pred, env
  | Not p ->
    let p, env = assign_id_pred p env in
    Not (p), env
  | And (p1, p2) ->
    let p1, env = assign_id_pred p1 env in
    let p2, env = assign_id_pred p2 env in
    And (p1, p2), env
  | Or (p1, p2) ->
    let p1, env = assign_id_pred p1 env in
    let p2, env = assign_id_pred p2 env in
    Or (p1, p2), env
  | LT (a1, a2) ->
    let a1, env = assign_id_arith a1 env in
    let a2, env = assign_id_arith a2 env in
    LT (a1, a2), env
  | LE (a1, a2) ->
    let a1, env = assign_id_arith a1 env in
    let a2, env = assign_id_arith a2 env in
    LE (a1, a2), env
  | GT (a1, a2) ->
    let a1, env = assign_id_arith a1 env in
    let a2, env = assign_id_arith a2 env in
    GT (a1, a2), env
  | GE (a1, a2) ->
    let a1, env = assign_id_arith a1 env in
    let a2, env = assign_id_arith a2 env in
    GE (a1, a2), env
  | EQ (a1, a2) ->
    let a1, env = assign_id_arith a1 env in
    let a2, env = assign_id_arith a2 env in
    EQ (a1, a2), env

let rec assign_id_statement (stmt : Syntax.s) (env : id M.t) =
  match stmt with
  | Assign (id, a) ->
    let a', env = assign_id_arith a env in
    let id' = gen_id id in
    let env = M.add id id' env in
    Assign (id', a'), env
  | Skip -> Skip, env
  | Block (s) ->
    let s, env = assign_id_statement s env in
    Block (s), env
  | Seq (s1, s2) ->
    let s1, env = assign_id_statement s1 env in
    let s2, env = assign_id_statement s2 env in
    Seq (s1, s2), env
  | While (p, s) ->
    let p', env = assign_id_pred p env in
    let s', env = assign_id_statement s env in
    While (p', s'), env
  | If (cond, s1, s2) ->
    let c, env = assign_id_pred cond env in
    let s1, env = assign_id_statement s1 env in
    let s2, env= assign_id_statement s2 env in
    If (c, s1, s2), env

let assign_id stmt =
  let stmt, _ = assign_id_statement stmt M.empty in
  stmt
