{
    open Lexing
    open Parser

    (* we can define LexerError and throw it manually in lexer, but in parser
       we can not throw it by ourself, parse error was defined and throwed in 
       automately generated code, so it is useless to define a ParseError*)
    exception LexerError of string


    (* error info provide the position of error for BOTH lexer and parser error *)
    let error_info alexbuf =
        let pos = alexbuf.lex_curr_p in
        let err_str = Printf.sprintf "Ln:%d, Col:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
        in
        let err_str =
            if String.length pos.pos_fname != 0 
            then String.concat ":" [pos.pos_fname; err_str]
            else err_str
        in
            err_str


    let next_line alexbuf =
        let pos = alexbuf.lex_curr_p in
        alexbuf.lex_curr_p <- 
        { pos with pos_bol = pos.pos_cnum;
                   pos_lnum = pos.pos_lnum + 1
        }
}

let int_rexp = '-'? ['0'-'9'] ['0'-'9']*
let id_rexp = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let blank = [' ' '\t']+
let newline = '\n' | '\r' | "\r\n"

rule read = 
    parse
    | '0'           { ZERO }
    | blank         { read lexbuf }
    | newline       { next_line lexbuf; read lexbuf }
    | '('           { L_PAREN }
    | ')'           { R_PAREN }
    | ';'           { SEMICOLEN }
    | "if"          { IF }
    | "then"        { THEN }
    | "else"        { ELSE }
    | "true"        { TRUE }
    | "false"       { FALSE }
    | "succ"        { SUCC }
    | "pred"        { PRED }
    | "iszero"      { IS_ZERO }
    | _             { raise (LexerError ("Error!!! Lexer error with Char: " ^ (Lexing.lexeme lexbuf)
                                          ^ " at " ^ (error_info lexbuf))) }
    | eof           { EOF }
