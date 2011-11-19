(* Copyright (C) 2011 by ForNeVeR

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE. *)

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

expressionParser.TermParser <-
    number
    <|> between (pstring "(") (pstring ")") expression

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

RegisterUnaryOperator "+" 1
RegisterUnaryOperator "-" 1

RegisterBinaryOperator "-" 1 Associativity.Left
RegisterBinaryOperator "+" 1 Associativity.Left
RegisterBinaryOperator "*" 2 Associativity.Left
RegisterBinaryOperator "/" 2 Associativity.Left
RegisterBinaryOperator "^" 3 Associativity.Right

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
