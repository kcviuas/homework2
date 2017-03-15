type expression =
    | True_exp 
    | False_exp 
    | If_exp    of expression * expression * expression
    | Zero_exp
    | Succ_exp  of expression
    | Pred_exp  of expression
    | Iszero_exp    of expression

type program =
    | A_program of expression list
