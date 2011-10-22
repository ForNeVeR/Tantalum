/// Tantalum command line input module.
module Tantalum.Input
    open System
    open FParsec
    open Tantalum.Core

    let expressionParser = new OperatorPrecedenceParser<Expression, unit, unit> ()
    let expression = expressionParser.ExpressionParser
    let number =
        regex @"[+-]?[\d]+(\.[\d]+)?([eE][+-]?[\d]+(\.[\d]+)?)?"
        |>> fun s -> SymbolicConstant s

    expressionParser.TermParser <-
        number
        <|> between (pstring "(") (pstring ")") expression

    let addBinaryOperator symbol priority expression =
        expressionParser.AddOperator <| InfixOperator (symbol, spaces, priority, Associativity.Left, expression)

    let rec substraction a b =
        BinaryOperator ("-", (-),
            (fun arg1 arg2 -> if arg1 = arg2 then zero else substraction a b),
            (a, b))

    let rec addition a b =
        BinaryOperator ("+", (+),
            (fun arg1 arg2 -> addition a b),
            (a, b))    
    
    addBinaryOperator "-" 1 substraction
    addBinaryOperator "+" 1 addition

    let parse message =
        match run expression message with
        | Success (result, _, _) -> result
        | Failure (msg, err, _)  -> failwith msg

    let repl =
        while true do
            Console.Write "> "
            let input = Console.ReadLine ()
            let result = 
                try
                    let operation = parse input |> simplify
                    let output = execute operation
                    String.Format ("{0} = {1}b", operation, output)
                with
                    | error -> error.Message
            printfn "%s" result

    repl
