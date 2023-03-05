type op = Add | Mul | Sub | Div | Mod | Pow
type expr = Int of int | Binop of op * expr * expr
