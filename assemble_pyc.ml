open Emit_pyc

let type_null      = '0'
let type_none      = 'N'
let type_false     = 'F'
let type_true      = 'T'
let type_stopiter  = 'S'
let type_ellipsis  = '.'
let type_int       = 'i'
let type_int64     = 'I'
let type_float     = 'f'
let type_string    = 's'
let type_interned  = 't'
let type_stringref = 'R'
let type_tuple    = '('
let type_list      = '['
let type_dict      = '{'
let type_code      = 'c'
let type_unicode   = 'u'
let type_unknown   = '?'
let type_set       = '<'
let type_frozenset = '>'

let marshal_null oc _ =
  Printf.fprintf oc "%c" type_null

let marshal_none oc _ =
  Printf.fprintf oc "%c" type_none

let marshal_bool oc b =
  if b then Printf.fprintf oc "%c" type_true
  else Printf.fprintf oc "%c" type_false

let put_int oc n =
  let a = Char.chr (Int.logand n 0xff) in
  let n = Int.shift_right n 8 in
  let b = Char.chr (Int.logand n 0xff) in
  let n = Int.shift_right n 8 in
  let c = Char.chr (Int.logand n 0xff) in
  let n = Int.shift_right n 8 in
  let d = Char.chr (Int.logand n 0xff) in
  Printf.fprintf oc "%c" a;
  Printf.fprintf oc "%c" b;
  Printf.fprintf oc "%c" c;
  Printf.fprintf oc "%c" d

let atom_int oc n =
  Printf.fprintf oc "%c" type_int;
  put_int oc n

let put_str oc s =
  put_int oc (String.length s);
  Printf.fprintf oc "%s" s

let atom_str oc s =
  Printf.fprintf oc "%c" type_string;
  put_int oc (String.length s);
  Printf.fprintf oc "%s" s

let marshal_int oc n = atom_int oc n

let marshal_str oc s = atom_str oc s

let marshal_tuple oc f lst =
  Printf.fprintf oc "%c" type_tuple;
  put_int oc (List.length lst);
  List.iter (fun x -> f oc x) lst

let marshal_pycode_dummy oc pycode =
  Printf.fprintf oc "%c" type_code;
  put_int oc 0;                  (* argcount *)
  put_int oc 0;                  (* nlocals *)
  put_int oc 0;                  (* stacksize *)
  put_int oc 64;                 (* flags *)
  marshal_str oc (Bytes.create 10 |> Bytes.to_string);  (* code *)
  marshal_tuple oc atom_int [1];       (* consts *)
  marshal_tuple oc atom_str ["x"];    (* names *)
  marshal_tuple oc atom_str [];    (* varnames *)
  marshal_tuple oc atom_str [];    (* freevars *)
  marshal_tuple oc atom_str [];    (* cellvars *)
  atom_str oc "test";
  atom_str oc "<module>";
  put_int oc 0;
  marshal_str oc (Bytes.create 1 |> Bytes.to_string)

let marshal_pycode oc = function
    PyCode (argcount, nlocals, stacksize, flags, code, consts, names, varnames, freevars, cellvars, filename, name, firstlineno, lnotab) ->
    Printf.fprintf oc "%c" type_code;
    put_int oc argcount;
    put_int oc nlocals;
    put_int oc stacksize;
    put_int oc flags;
    marshal_str oc (code |> Bytes.to_string);
    marshal_tuple oc atom_int consts;
    marshal_tuple oc atom_str names;
    marshal_tuple oc atom_str varnames;
    marshal_tuple oc atom_str freevars;
    marshal_tuple oc atom_str cellvars;
    atom_str oc filename;
    atom_str oc name;
    put_int oc firstlineno;
    marshal_str oc (lnotab |> Bytes.to_string)

let assemble oc pycode =
  marshal_pycode oc pycode
