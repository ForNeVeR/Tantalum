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

type PatternMatcher (patterns : Pattern seq) =
    let rec patternReplace pattern (variables : VariableDict) =
        match pattern with
        | (Template (Variable var)) -> variables.[var]
        | Constant _ as c           -> c
        | Function (f, args)        -> Function (f, Seq.map (fun pat -> patternReplace pat variables) args)
        | _                         -> failwith "Invalid or unmatched pattern."

    let rec mapVariables (pattern : ExecutionTree) (expression : ExecutionTree) (variables : VariableDict) : unit =
        match (pattern, expression) with
        | (Template (Variable name), expr)                 -> variables.Add (name, expr)
        | (Function (_, args1),       Function (_, args2)) ->
            Seq.iter2 (fun pat arg -> mapVariables pat arg variables) args1 args2
        | _                                                -> () 

    let rec straightMatch (pattern : ExecutionTree) (expression : ExecutionTree) : bool =
        match (pattern, expression) with
        | (p,                       e) when p = e     -> true
        | (Template Anything,       _)                -> true
        | (Template (Variable var), _)                -> true
        | (Template Zero,           e)
            when e = Constant (Symbolic (Symbol "0")) -> true
        | (Template One,            e)
            when e = Constant (Symbolic (Symbol "1")) -> true
        | (Function (func1, args1), Function (func2, args2))
            when func1 = func2                        ->
            Seq.map2 straightMatch args1 args2
            |> Seq.forall (fun b -> b)
        | _                                           -> false

    let rec deepMatchAny (expression : ExecutionTree) : ExecutionTree option =
        let matchedPattern = 
            patterns
            |> Seq.map (fun pattern -> (pattern, straightMatch pattern.Left expression))
            |> Seq.tryFind snd

        match matchedPattern with
        | Some (pattern, _) ->
            let variables = new VariableDict ()
            mapVariables pattern.Right expression variables
            Some (patternReplace pattern.Right variables)
        | None              ->
            match expression with
            | Function (f, args) ->
                let results = Seq.map (fun arg -> (arg, deepMatchAny arg)) args
                if Seq.exists (snd >> Option.isSome) results then
                    Some (Function (f, results |> Seq.map (fun result ->
                        match result with
                        | (_,   Some res) -> res
                        | (arg, None)     -> arg)))
                else
                    None                
            | _                  -> None

    member matcher.Match (expression : ExecutionTree) : ExecutionTree =
        let mutable proceed = true
        let mutable currentExpression = expression
        while proceed do
            match deepMatchAny currentExpression with
            | Some newExpression ->
                currentExpression <- newExpression
                proceed <- true
            | None ->
                proceed <- false
        currentExpression
        