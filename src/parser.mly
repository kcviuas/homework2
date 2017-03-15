%token L_PAREN
%token R_PAREN
%token SEMICOLEN

%token IF
%token THEN
%token ELSE
%token IS_ZERO
%token SUCC
%token PRED
%token TRUE
%token FALSE
%token ZERO

%token EOF

%start <Ast.program> enterPoint
%%

enterPoint:
    | ast = nt_program; EOF         { ast }

nt_program:
    | ast = separated_list (SEMICOLEN, nt_exp)
                                    { Ast.A_program ast }

nt_exp:
    | TRUE                          { Ast.True_exp }
    | FALSE                         { Ast.False_exp }
    | ZERO                          { Ast.Zero_exp }
    | IF; ast1 = nt_exp; THEN; ast2 = nt_exp; ELSE; ast3 = nt_exp
                                    { Ast.If_exp (ast1,ast2,ast3) }
    | L_PAREN; SUCC; ast = nt_exp; R_PAREN
                                    { Ast.Succ_exp ast }
    | L_PAREN; PRED; ast = nt_exp; R_PAREN
                                    { Ast.Pred_exp ast }
    | IS_ZERO; ast = nt_exp
                                    { Ast.Iszero_exp ast }