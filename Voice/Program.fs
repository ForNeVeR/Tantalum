/// Tantalum command line input module.
module Tantalum.Voice

open System
open FParsec
open Tantalum
open Tantalum.Executor

let private expressionParser = new OperatorPrecedenceParser<Expression, unit, unit> ()
let private expression = expressionParser.ExpressionParser
let private number =
    regex @"[\d]+(\.[\d]+)?([eE][+-]?[\d]+)?"
    |>> fun s -> Constant <| Symbol.Create s

let private tokenParser =
    (spaces >>. number .>> spaces)
    <|> between (pstring "(") (pstring ")") expression

expressionParser.TermParser <- tokenParser

/// Registers prefix unary operator for use in input stream.
let RegisterUnaryOperator (symbol : string) (priority : int) : unit =
    expressionParser.AddOperator
    <| PrefixOperator (symbol, spaces, priority, false,
                        fun a -> Function ({Id = symbol; Arity = 1}, [a]))

/// Registers binary operator for use in input stream.
let RegisterBinaryOperator (symbol : string) (priority : int) (associativity : Associativity) =
    expressionParser.AddOperator
    <| InfixOperator (symbol, spaces, priority, associativity,
                        fun a b -> Function ({Id = symbol; Arity = 2}, [a; b]))

let RegisterUnaryFunction (name : string) =
    expressionParser.AddOperator
    <| PrefixOperator (name, spaces, 3, false,
                        fun expr -> Function ({Id = name; Arity = 1}, [expr]))

RegisterUnaryOperator "+" 1
RegisterUnaryOperator "-" 1

RegisterBinaryOperator "-" 1 Associativity.Left
RegisterBinaryOperator "+" 1 Associativity.Left
RegisterBinaryOperator "*" 2 Associativity.Left
RegisterBinaryOperator "/" 2 Associativity.Left
RegisterBinaryOperator "^" 3 Associativity.Right

RegisterUnaryFunction "sin"
RegisterUnaryFunction "cos"
RegisterUnaryFunction "tg"
RegisterUnaryFunction "ctg"

/// Parses input string and returns corresponding Expression.
let Parse (input : string) : Expression =
    match run expression input with
    | Success (result, _, _) -> result
    | Failure (msg, err, _)  -> failwith msg

let private executor = executor ()

StandardOperations.Register executor

let private repl =
    while true do
        Console.Write "> "
        let input = Console.ReadLine ()
        let result = 
            try
                let inputExpr = Parse input
                let optimizedExpr = executor.CalculateSymbolic inputExpr
                let output = executor.CalculateBinary optimizedExpr
                sprintf "%s = %s = %sb" (inputExpr.ToString ()) (optimizedExpr.ToString ()) (output.ToString ())
            with
                | error -> error.Message
        printfn "%s" result

repl
