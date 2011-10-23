module Tantalum.Operations
    open Tantalum.Core

    let rec Substraction a b =
        BinaryOperator ("-", (-),
            (fun arg1 arg2 ->
                if arg1 = arg2
                then SymbolicZero
                else Substraction arg1 arg2),
            (a, b))

    let rec Addition a b =
        BinaryOperator ("+", (+),
            (fun arg1 arg2 -> Addition arg1 arg2),
            (a, b))  
