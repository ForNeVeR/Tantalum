/// Tantalum command line input module.
module Tantalum.Input
    open System
    open FParsec
    open Tantalum.Core

    let expressionParser = new OperatorPrecedenceParser<Node, unit, unit> ()
    let expression = expressionParser.ExpressionParser
    let number =
        regex @"[+-]?[\d]+(\.[\d]+)?([eE][+-]?[\d]+(\.[\d]+)?)?"
        |>> fun s -> Constant <| Symbol s

    expressionParser.TermParser <-
        number
        <|> between (pstring "(") (pstring ")") expression

    expressionParser.AddOperator
    <| InfixOperator ("-", spaces, 1, Associativity.Left,
        fun a b -> Operation (Substraction, a, b))

    expressionParser.AddOperator
    <| InfixOperator ("+", spaces, 1, Associativity.Left,
         fun a b -> Operation (Addition, a, b))

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
                    let output = calculate operation
                    String.Format ("{0} = {1}d", operation, output)
                with
                    | error -> error.Message
            printfn "%s" result

    repl
