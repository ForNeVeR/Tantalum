/// Tantalum command line input module.
module Tantalum.Input
    open System
    open FParsec
    open Tantalum.Core

    let parse message =
        Operation (Addition, Constant (Double 2.0), Constant (Symbol "2"))

    let rec repl =
        Console.Write "> "
        let input = Console.ReadLine ()
        let operation = parse input
        let output = calculate operation
        Console.WriteLine output

    repl