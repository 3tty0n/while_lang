%{

%}

%token BEGIN END
%token SEMICOLON
%token TRUE FALSE NOT
%token AND OR
%token PLUS MINUS TIMES DIVIDE
%token EQ LT GT LE GE
%token ASSIGN
%token WHILE DO
%token SKIP
%token IF THEN ELSE
%token <int> NUMBER           /* 整数には int 型の値が伴うことを表す */
%token <string> VARIANT
%token EOF                    /* 入力の終わりを表わす */

/* エントリーポイント (開始記号) の定義 */
%start start

/* 非終端記号の型を宣言 */
%type <Syntax.s> start

/* 演算子の優先順位を指定する (低い方から高い方へ) */
/* 結合なし  */
%nonassoc UNARY
%right prec_assign
%left PLUS MINUS
%left TIMES DIVIDE
%left AND OR

/* %% は省略不可 */
%%

start:
| statement EOF  { $1 }

statement:
| VARIANT ASSIGN arith  { Assign ($1, $3) }
| SKIP { Skip }
| statement SEMICOLON statement { Seq ($1, $3) }
| IF predicate THEN statement ELSE statement { If ($2, $4, $6) }
| WHILE predicate DO statement { While ($2, $4) }

predicate:
| TRUE  { True }
| FALSE { False }
| predicate AND predicate { And ($1, $3) }
| predicate OR predicate { Or ($1, $3) }
| arith LT arith { LT ($1, $3) }
| arith LE arith { LE ($1, $3) }
| arith GT arith { GT ($1, $3) }
| arith GE arith { GE ($1, $3) }
| arith EQ arith { EQ ($1, $3) }

arith:
| NUMBER { Num ($1) }
| VARIANT { Var ($1) }
| arith PLUS arith { Add ($1, $3) }
| arith MINUS arith { Sub ($1, $3) }
| arith TIMES arith { Mul ($1, $3) }
| arith DIVIDE arith { Div ($1, $3) }
