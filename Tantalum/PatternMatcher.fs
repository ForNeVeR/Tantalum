namespace Tantalum

open System.Collections.Generic

type private VariableDict = Dictionary<string, Expression>

/// Type for doing pattern matching on expression.
type PatternMatcher (executor : IExecutor, simplificationPatterns : Pattern seq, normalizationPatterns : Pattern seq) =
    let memoize f =
        let cache = new Dictionary<_, _>()
        (fun x -> match cache.TryGetValue(x) with
                    | true, y -> y
                    | _       -> let v = f(x)
                                 cache.Add(x, v)
                                 v)

    let rec patternReplace pattern (variables : VariableDict) =
        match pattern with
        | Template (Variable var) -> variables.[var]
        | Constant _ as c         -> c
        | Function (f, args)      -> Function (f, List.map (fun pat -> patternReplace pat variables) args)
        | _                       -> failwith "Invalid or unmatched pattern."

    let rec mapVariables pattern expression (variables : VariableDict) : unit =
        match pattern, expression with
        | Template (Variable name), expr ->
            match variables.TryGetValue name with
            | false, _                      -> variables.[name] <- expr
            | true, value when value = expr -> ()
            | _                             -> failwithf "Cannot redefine variable %s." name
        | Function (f1, args1),     Function (f2, args2)
            when f1 = f2                 ->
            List.iter2 (fun pat arg -> mapVariables pat arg variables) args1 args2
        | _                              -> ()

    let rec straightMatch pattern expression (variables : VariableDict) : bool =
        match pattern, expression with
        | Constant _ as c, e
            when executor.CalculateSymbolic e = c      -> true
        | Template Anything, _                         -> true
        | Template (Variable var), e
            when not (variables.ContainsKey var)       ->
            variables.Add(var, e)
            true
        | Template (Variable var), e
            when snd (variables.TryGetValue var) = e   -> true
        | Function (func1, args1), Function (func2, args2)
            when func1 = func2                         ->
            List.map2 (fun a b -> straightMatch a b variables) args1 args2
            |> List.forall (fun b -> b)
        | _                                            -> false

    let deepMatchAllImpl(patterns, expression) : Expression seq =
        let rec matchPattern pat expr =
            let variables = new VariableDict()
            if straightMatch pat.Left expr variables then
                let variables = new VariableDict ()
                mapVariables pat.Left expr variables
                Some (patternReplace pat.Right variables)
            else
                match expr with
                | Function (f, args) ->
                    let results = List.map (fun arg -> arg, matchPattern pat arg) args
                    if List.exists (snd >> Option.isSome) results then
                        Some (Function (f, results |> List.map (fun result ->
                            match result with
                            | _,   Some res -> res
                            | arg, None     -> arg)))
                    else
                        None                
                | _                  -> None

        patterns
        |> Seq.map (fun pat -> matchPattern pat expression)
        |> Seq.filter Option.isSome
        |> Seq.map Option.get

    let deepMatchAll = memoize deepMatchAllImpl

    let deepMatchAnyImpl (patterns, expression) =
         deepMatchAll (patterns, expression)
         |> Seq.tryFind (fun _ -> true)

    let deepMatchAny = memoize deepMatchAnyImpl

    /// Trying to match all available patterns on expression.
    member matcher.Match (expression : Expression) : Expression =
        let simplify expr = deepMatchAny (simplificationPatterns, expr)
        
        let normalForms expr =
            let known = new System.Collections.Generic.HashSet<Expression> (HashIdentity.Structural)
            known.Add expr |> ignore

            let rec generate expr : Expression seq =
                let added = deepMatchAll (normalizationPatterns, expr)
                let newExprSequences =
                    added
                    |> Seq.filter known.Add
                    |> Seq.map generate
                Seq.concat [added; newExprSequences
                                   |> Seq.concat]
            
            let generated = generate expr
            Seq.concat [seq { yield expr }; generated]
            
        let rec simplificationLoop expr =
            let formsSimplified =
                normalForms expr
                |> Seq.map (fun expr -> expr, simplify expr)
            let simplifications =
                formsSimplified
                |> Seq.map snd
                |> Seq.filter Option.isSome
                |> Seq.map Option.get
            let result =
                simplifications
                |> Seq.tryFind (fun _ -> true)
            match result with
            | Some r -> simplificationLoop r
            | None   -> expr

        simplificationLoop expression
