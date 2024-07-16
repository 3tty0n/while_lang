{
    (* Lexer が仕様する変数、関数などの定義 *)
    open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let letter = lower | upper

rule token = parse
| space+ { token lexbuf }
| '+'  { PLUS }
| '-'  { MINUS }
| '*'  { TIMES }
| '/'  { DIVIDE }
| "<"  { LT }
| "<=" { LE }
| ">"  { GT }
| ">=" { GE }
| "==" { EQ }
| ":=" { ASSIGN }
| ';'  { SEMICOLON }
| "begin" { BEGIN }
| "end"   { END }
| "while" { WHILE }
| "do"    { DO }
| "true"  { TRUE }
| "not"   { NOT }
| "false" { FALSE }
| "and"   { AND }
| "or"    { OR }
| "skip"  { SKIP }
| "if"    { IF }
| "then"  { THEN }
| "else"  { ELSE }
| digit+  { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
| eof     { EOF }
| _       { failwith ("unknown token: " ^ Lexing.lexeme lexbuf) }
