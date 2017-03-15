open Core.Std
open Lexing

open Parser
open Lexer
open Ast

exception RuntimeError of string


let getAst = 
    fun str ->
    let alexbuf = Lexing.from_string str in
    try
    Parser.enterPoint Lexer.read alexbuf
    with
    | Lexer.LexerError str -> print_string str; exit 1
    | Parser.Error -> print_string ("Oops!!! parser error with char: " ^ (Lexing.lexeme alexbuf)
                                        ^ " at: " ^ (Lexer.error_info alexbuf)); exit 1


(*-------------------------------------------------------*)
(* expressed value & denoted value *)
(* environment & store *)

type expval =
    | Num_val   of int
    | Bool_val  of bool
    | Wrong


(* Eval *)
(*-------------------------------------------------------*)

let rec value_of =
    fun (exp:Ast.expression) :expval ->
    match exp with
    | True_exp -> Bool_val true
    | False_exp -> Bool_val false
    | If_exp (exp1, exp2, exp3) -> let v1 = value_of exp1 in
                                  (match v1 with
                                   | Bool_val fg -> if fg then (value_of exp2) else (value_of exp3)
                                   | _ -> Wrong)
    | Zero_exp -> Num_val 0
    | Succ_exp exp1 -> let v = value_of exp1 in
                      (match v with
                       | Num_val rv -> Num_val (rv + 1)
                       | _ -> Wrong)
    | Pred_exp exp1 -> let v = value_of exp1 in
                      (match v with
                       | Num_val rv -> if rv > 0 then (Num_val (rv-1)) else (Num_val 0)
                       | _ -> Wrong)
    | Iszero_exp exp1 -> let v = value_of exp1 in
                      (match v with
                       | Num_val rv -> if rv = 0 then (Bool_val true) else (Bool_val false)
                       | _ -> Wrong)


let value_of_program =
  fun pgm ->
    match pgm with
    |A_program expli -> List.map expli value_of


let print_expval =
  fun expv ->
    match expv with
    | Num_val v -> print_int v; print_newline ()
    | Bool_val v -> if v = true then print_string "true\n" else print_string "false\n"
    | Wrong     -> print_string "Error, NoRuleApplies\n"


let () =
    let fileName = Sys.argv.(1) in
    let file = In_channel.create fileName in
    let str = In_channel.input_all file in
    In_channel.close file;
    let ast = getAst str in
    let _ = List.map (value_of_program ast) print_expval in ()


