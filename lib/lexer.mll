{
    open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+

rule read = 
    parse
    | white { read lexbuf }
    | '+' { PLUS }
    | '*' { TIMES }
    | '-' { MINUS }
    | '/' { DIV }
    | '%' { MOD }
    | '^' { POW }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | "exit" { EXIT }
    | eof { EOF } 
