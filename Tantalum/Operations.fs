module Tantalum.Operations
    open Tantalum.Core

    let rec Substraction a b =
        BinaryOperator ("-", (-),
            (fun arg1 arg2 ->
                if arg1.Equals arg2
                then SymbolicZero
                else Substraction arg1 arg2),
            (a, b))

    let rec Addition a b =
        BinaryOperator ("+", (+),
            (fun arg1 arg2 -> Addition arg1 arg2),
            (a, b))  

    let rec Multiplication a b =
        BinaryOperator ("*", (*),
            (fun arg1 arg2 ->
                match (arg1, arg2) with
                | (_, _) when EqualsOne arg1 -> arg2
                | (_, _) when EqualsOne arg2 -> arg1
                | _                          -> Multiplication arg1 arg2),
            (a, b))
