open Calc.Ast

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Calc.Parser.prog Calc.Lexer.read lexbuf in
  ast

let string_of_val e =
  match e with
  | Int i -> string_of_int i
  | Binop _ -> failwith "precondition violated"

let is_value = function Int _ -> true | Binop _ -> false

let rec step = function
  | Int _ -> failwith "no step"
  | Binop (op, e1, e2) when is_value e1 && is_value e2 -> eval_binop op e1 e2
  | Binop (op, e1, e2) when is_value e1 -> Binop (op, e1, step e2)
  | Binop (op, e1, e2) -> Binop (op, step e1, e2)

and eval_binop op v1 v2 =
  match (op, v1, v2) with
  | Add, Int i1, Int i2 -> Int (i1 + i2)
  | Mul, Int i1, Int i2 -> Int (i1 * i2)
  | Sub, Int i1, Int i2 -> Int (i1 - i2)
  | Div, Int i1, Int i2 -> Int (i1 / i2)
  | Mod, Int i1, Int i2 -> Int (i1 mod i2)
  | Pow, Int i1, Int i2 ->
      Int (int_of_float (float_of_int i1 ** float_of_int i2))
  | _ -> failwith "precondition violated"

let rec eval e = if is_value e then e else e |> step |> eval
let interpret s = s |> parse |> eval |> string_of_val

let () =
  print_endline "Calc v1.0";
  print_endline "*******************";
  while true do
    try
      print_string "> ";
      let s = read_line () in
      print_endline (interpret s)
    with _ -> print_endline "Invalid syntax"
  done
