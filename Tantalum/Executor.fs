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

    type Executor () =
        let functions = new Dictionary<Function, Functor> ()
        let simplificationPatterns = new HashSet<Pattern> ()

        member executor.AddUnaryFunction (func : Function) (applyFunctor : double -> double) =
            if func.Arity = 1
            then
                functions.[func] <-
                    fun args ->
                        match args with
                        | [arg] -> applyFunctor arg
                        | _     -> failwith "Wrong number of arguments for unary function."
            else failwith "Wrong unary function definition."

        member executor.AddBinaryFunction (func : Function) (applyFunctor : double * double -> double) =
            if func.Arity = 2
            then
                functions.[func] <-
                    fun args ->
                        match args with
                        | [arg1; arg2] -> applyFunctor (arg1, arg2)
                        | _            -> failwith "Wrong number of arguments for binary function."
            else failwith "Wrong binary function definition."

        member executor.AddSimplificationPattern (pattern : Pattern) = 
            simplificationPatterns.Add pattern |> ignore
            ()

        member executor.AddNormalizationPattern (pattern : Pattern) = ()

        member executor.CalculateSymbolic (expression: ExecutionTree) =
            let matcher = new PatternMatcher (executor, simplificationPatterns)
            matcher.Match expression
           
        member executor.CalculateBinary (expression: ExecutionTree) =
            match expression with
            | Constant (Double d)               -> d
            | Constant (Symbolic s)             -> s.ToBinary ()
            | Function (func, args)
                when func.Arity = args.Count () ->
                    let apply = functions.[func]
                    Seq.map executor.CalculateBinary args
                    |> Seq.toList
                    |> apply 
            | Function _                        -> failwith "Wrong number of arguments."
            | Template _                        -> failwith "Attempt to calculate template expression."
    and PatternMatcher (executor : Executor, patterns : Pattern seq) =
        let rec patternReplace pattern (variables : IDictionary<string, ExecutionTree>) =
                match pattern with
                | (Template (Variable var)) -> variables.[var]
                | Constant _ as c           -> c
                | Function (f, args)        -> Function (f, Seq.map (fun pat -> patternReplace pat variables) args)
                | _                         -> failwith "Invalid or unmatched pattern."

        let rec patternMatch pattern expression =
            let variables = new Dictionary<string, ExecutionTree> ()
            match (pattern, expression) with
            | (p,                       e) when p = e     -> (true, variables)
            | (Template Anything,       _)                -> (true, variables)
            | (Template (Variable var), e)                ->
                if variables.ContainsKey var
                then
                    match variables.[var] with
                    | expr when expr = e -> (true, variables)
                    | _                  -> (false, variables)
                else
                    variables.[var] <- e
                    (true, variables)
            | (Template Zero,           e)
                when e = Constant (Symbolic (Symbol "0")) -> (true, variables)
            | (Template One,            e)
                when e = Constant (Symbolic (Symbol "1")) -> (true, variables)
            | (Function (f1, patternArgs),
                Function (f2, funcArgs)) when f1 = f2     ->
                    let results = Seq.map2 patternMatch patternArgs funcArgs
                    if Seq.forall (fun res -> fst res) results
                    then
                        results
                        |> Seq.map snd
                        // TODO: Check whether variables dict haven't variables.[v.Key] set already.
                        |> Seq.iter (fun var -> var |> Seq.iter (fun v -> variables.[v.Key] <- v.Value))
                        (true, variables)
                    else (false, variables)
            | (_,                       _)                -> (false, variables)

        member matcher.Match expression : ExecutionTree =
            let result =
                patterns
                |> Seq.map (fun pattern -> (patternMatch pattern.Left expression, pattern.Right))
                |> Seq.tryFind (fun res -> fst <| fst res)
            match result with
            | Some value -> patternReplace (snd value) (snd <| fst value)
            | None       -> expression
