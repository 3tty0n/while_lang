open Pycode

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

let marshal_w_object oc w_object =
  match w_object with
  | W_Int n -> atom_int oc n
  | W_String s -> atom_str oc s
  | W_None -> marshal_none oc ()
  | W_True -> marshal_bool oc true
  | W_False -> marshal_bool oc false

let marshal_tuple oc lst =
  Printf.fprintf oc "%c" type_tuple;
  put_int oc (List.length lst);
  List.iter (fun x -> marshal_w_object oc x) lst

let marshal_pycode oc = function
    PyCode p ->
    Printf.fprintf oc "%c" type_code;
    put_int oc p.argcount;
    put_int oc p.nlocals;
    put_int oc p.stacksize;
    put_int oc p.flags;
    marshal_str oc (p.code |> Bytes.to_string);
    marshal_tuple oc p.consts;
    marshal_tuple oc p.names;
    marshal_tuple oc p.varnames;
    marshal_tuple oc p.freevars;
    marshal_tuple oc p.cellvars;
    atom_str oc p.filename;
    atom_str oc p.name;
    put_int oc p.firstlineno;
    marshal_str oc (p.lnotab |> Bytes.to_string)

let assemble oc pycode =
  marshal_pycode oc pycode
