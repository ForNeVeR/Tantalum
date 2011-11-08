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

type private VariableDict = Dictionary<string, ExecutionTree>

/// Type for doing pattern matching on expression.
type PatternMatcher (executor: IExecutor, patterns : Pattern seq) =
    let rec patternReplace pattern (variables : VariableDict) =
        match pattern with
        | (Template (Variable var)) -> variables.[var]
        | Constant _ as c           -> c
        | Function (f, args)        -> Function (f, List.map (fun pat -> patternReplace pat variables) args)
        | _                         -> failwith "Invalid or unmatched pattern."

    let rec mapVariables (pattern : ExecutionTree) (expression : ExecutionTree) (variables : VariableDict) : unit =
        match (pattern, expression) with
        | (Template (Variable name), expr) ->
            match variables.TryGetValue name with
            | (false, _)                      -> variables.[name] <- expr
            | (true, value) when value = expr -> ()
            | _                               -> failwithf "Cannot redefine variable %s." name                 
        | (Function (f1, args1),     Function (f2, args2))
            when f1 = f2                   ->
            List.iter2 (fun pat arg -> mapVariables pat arg variables) args1 args2
        | _                                -> () 

    let rec straightMatch (pattern : ExecutionTree) (expression : ExecutionTree) : bool =
        match (pattern, expression) with
        | (Constant _ as c,         e) when executor.CalculateSymbolic e = c -> true
        | (Template Anything,       _) -> true
        | (Template (Variable var), _) -> true
        | (Function (func1, args1), Function (func2, args2))
            when func1 = func2         ->
            List.map2 straightMatch args1 args2
            |> List.forall (fun b -> b)
        | _                            -> false

    let rec deepMatchAny (expression : ExecutionTree) : ExecutionTree option =
        let matchedPattern = 
            patterns
            |> Seq.map (fun pattern -> (pattern, straightMatch pattern.Left expression))
            |> Seq.tryFind snd

        match matchedPattern with
        | Some (pattern, _) ->
            let variables = new VariableDict ()
            mapVariables pattern.Left expression variables
            Some (patternReplace pattern.Right variables)
        | None              ->
            match expression with
            | Function (f, args) ->
                let results = List.map (fun arg -> (arg, deepMatchAny arg)) args
                if List.exists (snd >> Option.isSome) results then
                    Some (Function (f, results |> List.map (fun result ->
                        match result with
                        | (_,   Some res) -> res
                        | (arg, None)     -> arg)))
                else
                    None                
            | _                  -> None

    /// Trying to match all available patterns on expression.
    member matcher.Match (expression : ExecutionTree) : ExecutionTree =
        match deepMatchAny expression with
        | Some newExpression -> matcher.Match newExpression
        | None -> expression
