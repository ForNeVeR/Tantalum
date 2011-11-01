/// Tantalum command line input module.
module VoiceOfTantalum.Program
    open System
    open FParsec
    open Tantalum.Core
    open Tantalum.Operations

    let private expressionParser = new OperatorPrecedenceParser<Expression, unit, unit> ()
    let private expression = expressionParser.ExpressionParser
    let private number =
        regex @"[+-]?[\d]+(\.[\d]+)?([eE][+-]?[\d]+(\.[\d]+)?)?"
        |>> fun s -> SymbolicConstant s

    expressionParser.TermParser <-
        number
        <|> between (pstring "(") (pstring ")") expression

    /// Registers binary operator for use in input stream.
    let RegisterBinaryOperator symbol priority associativity expression =
        expressionParser.AddOperator <| InfixOperator (symbol, spaces, priority, associativity, expression)

    RegisterBinaryOperator "-" 1 Associativity.Left Substraction
    RegisterBinaryOperator "+" 1 Associativity.Left Addition
    RegisterBinaryOperator "*" 2 Associativity.Left Multiplication

    /// Parses message and returns corresponding Expression.
    let Parse message =
        match run expression message with
        | Success (result, _, _) -> result
        | Failure (msg, err, _)  -> failwith msg

    let private repl =
        while true do
            Console.Write "> "
            let input = Console.ReadLine ()
            let result = 
                try
                    let operation = Parse input |> Simplify
                    let output = Execute operation
                    String.Format ("{0} = {1}b", operation, output)
                with
                    | error -> error.Message
            printfn "%s" result

    repl
