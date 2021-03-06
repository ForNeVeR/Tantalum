﻿module Tantalum.Executor

open System.Collections.Generic
open System.Linq

type CalcFunction = Symbol list -> Expression
type ApplyFunction = double list -> double

/// Executor is the Tantalum core type. Objects of this type are used to
/// simplify and calculate expressions.
let executor () =
    let calcFunctions = new Dictionary<Function, CalcFunction> ()
    let applyFunctions = new Dictionary<Function, ApplyFunction> ()
    let simplificationPatterns = new HashSet<Pattern> ()
    let normalizationPatterns = new HashSet<Pattern> ()

    {new IExecutor with
        /// Adds unary function to internal storage.
        member executor.AddUnaryFunction func calcFunctor applyFunctor =
            if func.Arity = 1
            then
                calcFunctions.[func] <-
                    fun args ->
                        match args with
                        | [arg] -> calcFunctor arg
                        | _     -> failwith "Wrong number of arguments for unary function."
                applyFunctions.[func] <-
                    fun args ->
                        match args with
                        | [arg] -> applyFunctor arg
                        | _     -> failwith "Wrong number of arguments for unary function."
            else failwith "Wrong unary function definition."

        /// Adds binary function to internal storage.
        member executor.AddBinaryFunction func calcFunctor applyFunctor =
            if func.Arity = 2
            then
                calcFunctions.[func] <-
                    fun args ->
                        match args with
                        | [arg1; arg2] -> calcFunctor (arg1, arg2)
                        | _            -> failwith "Wrong number of arguments for binary function."
                applyFunctions.[func] <-
                    fun args ->
                        match args with
                        | [arg1; arg2] -> applyFunctor (arg1, arg2)
                        | _            -> failwith "Wrong number of arguments for binary function."
            else failwith "Wrong binary function definition."

        /// Adds simplification pattern to internal storage.
        member executor.AddSimplificationPattern pattern = 
            simplificationPatterns.Add pattern
            |> ignore

        /// Adds normalization pattern to internal storage.
        member executor.AddNormalizationPattern pattern =
            normalizationPatterns.Add pattern
            |> ignore

        /// Simplifies an expression.
        member executor.CalculateSymbolic expression =
            let rec evaluate expr = 
                let onlySymbols exprs =
                    exprs
                    |> List.forall (fun expr ->
                        match expr with
                        | Constant _ -> true
                        | _          -> false)

                match expr with
                | Function (func, args) when onlySymbols args ->
                    args
                    |> List.map (fun expr ->
                        match expr with
                        | Constant symbol -> symbol
                        | _               -> failwith "Not a symbol.")
                    |> calcFunctions.[func]
                | Function (func, args)                       ->
                    Function (func, args
                                    |> List.map evaluate)
                | Constant _ as c                             -> c
                | _ as other                                  -> other

            let rec calculate expr =            
                let evaluated = evaluate expr
                if evaluated = expr then
                    let matcher = new PatternMatcher (executor, simplificationPatterns, normalizationPatterns)
                    let simplified = matcher.Match expr
                    if simplified = expr then
                        expr
                    else
                        calculate simplified
                else
                    calculate evaluated

            calculate expression

        /// Calculates expression in binary.
        member executor.CalculateBinary expression =
            let rec calculate expression =
                match expression with
                | Constant c                        -> c.ToBinary ()
                | Function (func, args)
                    when func.Arity = args.Count () ->
                        let apply = applyFunctions.[func]
                        Seq.map calculate args
                        |> Seq.toList
                        |> apply
                | Function _                        -> failwith "Wrong number of arguments."
                | Template _                        -> failwith "Attempt to calculate template expression."
            calculate expression
    }
