%{

%}

%token BEGIN END
%token PLUS MINUS TIMES DIVIDE
%token EQ LT GT LE GE
%token ASSIGN
%token WHILE DO
%token IF ELSE
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

/* %% は省略不可 */
%%

start:
| statement EOF  { $1 }

statement:
| VARIANT ASSIGN arith  { Assign ($1, $3) }

arith:
| NUMBER { Num ($1) }
| VARIANT { Var ($1) }
| arith PLUS arith { Add ($1, $3) }
| arith MINUS arith { Sub ($1, $3) }
| arith TIMES arith { Mul ($1, $3) }
| arith DIVIDE arith { Div ($1, $3) }
