%token EOF
%token LPAREN
%token RPAREN
%token <int> INT
%token PLUS
%token TIMES
%token MINUS
%token DIV
%token MOD
%token POW
%token EXIT

%start <Ast.expr> prog 

%%

prog:
    | e = expr; EOF { e }
    | EXIT { exit(0) }
    ;

expr:
    | term { $1 }
    | expr PLUS term { Ast.Binop(Add, $1, $3) }
    | expr MINUS term { Ast.Binop(Sub,$1, $3) }
    ;

term: 
    | powterm { $1 }
    | expr TIMES powterm { Ast.Binop(Mul, $1, $3) }
    | expr DIV powterm { Ast.Binop(Div, $1, $3) }
    | expr MOD powterm { Ast.Binop(Mod, $1, $3) }
    ;

powterm:
    | factor { $1 }
    | factor POW powterm { Ast.Binop(Pow, $1,  $3) }

factor:
    | INT { Int($1) }
    | MINUS INT { Int(-1 * $2) }
    | LPAREN expr RPAREN { $2 }
    ;
