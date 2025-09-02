open Printf

open Virtual_stack

exception Error of string

let indent = ref 4

let make_indent oc =
  List.iter (fun _ -> Printf.fprintf oc " ") (List.init !indent (fun _ -> ()))

type env = { mutable globals: (string, unit) Hashtbl.t }

type cframe = { test: string; out: string }

let wasm_of_binop = function
  | PLUS  -> "i32.add"
  | MINUS -> "i32.sub"
  | TIMES -> "i32.mul"
  | DIV   -> "i32.div_s"
  | EQ    -> "i32.eq"
  | LT    -> "i32.lt_s"
  | LE    -> "i32.le_s"
  | GT    -> "i32.gt_s"
  | GE    -> "i32.ge_s"
  | AND   -> "i32.and"
  | OR    -> "i32.or"
  | _ -> invalid_arg "not a binop"

let collect_globals prog =
  let g = Hashtbl.create 8 in
  List.iter (function
      | RValue x | LPush x -> Hashtbl.replace g x ()
      | _ -> ()
    ) prog;
  g

let emit_instr (oc : out_channel) (ctrl : cframe list ref) (pending_store: string option ref) =
  let _ = make_indent oc in
  function
  | Push n      -> Printf.fprintf oc "i32.const %d\n" n
  | TRUE        -> Printf.fprintf oc "i32.const 1\n"
  | FALSE       -> Printf.fprintf oc "i32.const 0\n"
  | NOT         -> Printf.fprintf oc "i32.eqz\n"
  | PLUS | MINUS | TIMES | DIV
  | EQ | LT | LE | GT | GE
  | AND | OR as t -> Printf.fprintf oc "%s\n" (wasm_of_binop t)
  | RValue x    -> Printf.fprintf oc "global.get $%s\n" x
  | PRINT       -> Printf.fprintf oc "call $print\n"
  | LPush x     -> Printf.fprintf oc "global.set $%s\n" x
  | LabelTest (t, out) ->
     ctrl := {test=t; out=out} :: !ctrl;
     Printf.fprintf oc "(block $%s\n" out;
     make_indent oc;
     Printf.fprintf oc "(loop $%s\n" t
  | GoFalse out ->
     (* 直前までに cond がスタックにある想定 *)
     Printf.fprintf oc "i32.eqz\n";
     make_indent oc;
     Printf.fprintf oc "br_if $%s\n" out
  | GoTo t -> Printf.fprintf oc "br $%s\n" t
  | LabelOut (t, out) ->
     (* スタック整合性を軽く確認（任意） *)
     begin match !ctrl with
     | {test; out=_} :: rest when test = t ->
        ctrl := rest;
        (Printf.fprintf oc ") ;; loop\n";
         make_indent oc;
         Printf.fprintf oc ") ;; block\n")
     | _ ->
        raise (Error (Printf.sprintf "LabelOut mismatch for %s/%s" t out))
     end

let compile_expr_subset (oc : out_channel) (prog:t list) : unit =
  let g = collect_globals prog in
  let pending_store = ref None in
  let ctrl = ref [] in
  Printf.fprintf oc "(module\n";
  Printf.fprintf oc "  (import \"env\" \"print\" (func $print (param i32)))\n";
  Hashtbl.to_seq_keys g
  |> Seq.iter (fun x ->
         Printf.fprintf oc "  (global $%s (mut i32) (i32.const 0))\n" x);
  Printf.fprintf oc "  (func $main\n";
  List.iter (fun p ->
      emit_instr oc ctrl pending_store p
    ) prog;
  Printf.fprintf oc "  )\n";
  Printf.fprintf oc "\n  (export \"main\" (func $main))\n";
  Printf.fprintf oc ")\n"
