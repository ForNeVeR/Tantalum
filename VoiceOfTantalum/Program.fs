﻿/// Tantalum command line input module.
module VoiceOfTantalum.Program
    open System
    open FParsec
    open Tantalum

    let private expressionParser = new OperatorPrecedenceParser<ExecutionTree, unit, unit> ()
    let private expression = expressionParser.ExpressionParser
    let private number =
        regex @"[+-]?[\d]+(\.[\d]+)?([eE][+-]?[\d]+(\.[\d]+)?)?"
        |>> fun s -> Constant (Symbolic <| Symbol s)

    expressionParser.TermParser <-
        number
        <|> between (pstring "(") (pstring ")") expression

    /// Registers prefix unary operator for use in input stream.
    let RegisterUnaryOperator symbol priority =
        expressionParser.AddOperator
        <| PrefixOperator (symbol, spaces, priority, false,
                           fun a -> Function ({Id = symbol; Arity = 1}, [|a|]))

    /// Registers binary operator for use in input stream.
    let RegisterBinaryOperator symbol priority associativity =
        expressionParser.AddOperator
        <| InfixOperator (symbol, spaces, priority, associativity,
                          fun a b -> Function ({Id = symbol; Arity = 2}, [|a; b|]))

    RegisterUnaryOperator "+" 1
    RegisterUnaryOperator "-" 1

    RegisterBinaryOperator "-" 1 Associativity.Left
    RegisterBinaryOperator "+" 1 Associativity.Left
    RegisterBinaryOperator "*" 2 Associativity.Left
    RegisterBinaryOperator "/" 2 Associativity.Left
    RegisterBinaryOperator "^" 3 Associativity.Right

    /// Parses message and returns corresponding Expression.
    let Parse message =
        match run expression message with
        | Success (result, _, _) -> result
        | Failure (msg, err, _)  -> failwith msg

    let executor = new Executor ()

    executor.AddUnaryFunction {Id = "+"; Arity = 1} <| fun a -> a
    executor.AddUnaryFunction {Id = "-"; Arity = 1} <| fun a -> -a
    
    executor.AddBinaryFunction {Id = "+"; Arity = 2} <| fun (a, b) -> a + b
    executor.AddBinaryFunction {Id = "-"; Arity = 2} <| fun (a, b) -> a - b
    executor.AddBinaryFunction {Id = "*"; Arity = 2} <| fun (a, b) -> a * b
    executor.AddBinaryFunction {Id = "/"; Arity = 2} <| fun (a, b) -> a / b
    executor.AddBinaryFunction {Id = "^"; Arity = 2} <| Math.Pow

    let private repl =
        while true do
            Console.Write "> "
            let input = Console.ReadLine ()
            let result = 
                try
                    let operation = Parse input |> executor.CalculateSymbolic |> snd
                    let output = executor.CalculateBinary operation
                    sprintf "%A = %Ab" operation output
                with
                    | error -> error.Message
            printfn "%s" result

    repl
