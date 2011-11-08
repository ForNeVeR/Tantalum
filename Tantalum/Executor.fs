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

namespace Tantalum

open System.Collections.Generic
open System.Linq

type private Functor = double list -> double

/// Executor is the Tantalum core type. Objects of this type are used to
/// simplify and calculate expressions.
type Executor () =
    let functions = new Dictionary<Function, Functor> ()
    let simplificationPatterns = new HashSet<Pattern> ()

    /// Adds unary function to internal storage.
    member executor.AddUnaryFunction (func : Function) (applyFunctor : double -> double) : unit =
        if func.Arity = 1
        then
            functions.[func] <-
                fun args ->
                    match args with
                    | [arg] -> applyFunctor arg
                    | _     -> failwith "Wrong number of arguments for unary function."
        else failwith "Wrong unary function definition."

    /// Adds binary function to internal storage.
    member executor.AddBinaryFunction (func : Function) (applyFunctor : double * double -> double) : unit =
        if func.Arity = 2
        then
            functions.[func] <-
                fun args ->
                    match args with
                    | [arg1; arg2] -> applyFunctor (arg1, arg2)
                    | _            -> failwith "Wrong number of arguments for binary function."
        else failwith "Wrong binary function definition."

    /// Adds simplification pattern to internal storage.
    member executor.AddSimplificationPattern (pattern : Pattern) : unit = 
        simplificationPatterns.Add pattern
        |> ignore

    /// Adds normalization pattern to internal storage.
    member executor.AddNormalizationPattern (pattern : Pattern) : unit =
        ()

    /// Simplifies an expression.
    member executor.CalculateSymbolic (expression: ExecutionTree) : ExecutionTree =
        let matcher = new PatternMatcher (simplificationPatterns)
        matcher.Match expression

    /// Calculates expression in binary.
    member executor.CalculateBinary (expression: ExecutionTree) : double =
        let rec calculate expression =
            match expression with
            | Constant (Double d)               -> d
            | Constant (Symbolic s)             -> s.ToBinary ()
            | Function (func, args)
                when func.Arity = args.Count () ->
                    let apply = functions.[func]
                    Seq.map calculate args
                    |> Seq.toList
                    |> apply 
            | Function _                        -> failwith "Wrong number of arguments."
            | Template _                        -> failwith "Attempt to calculate template expression."
        calculate expression
