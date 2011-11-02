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
            // TODO: A crap! Rewrite this!
            let rec patternReplace pattern (variables : IDictionary<string, ExecutionTree>) =
                match pattern with
                | (Template (Variable var)) -> variables.[var]
                | Constant _ as c           -> c
                | Function (f, args)        -> Function (f, Seq.map (fun pat -> patternReplace pat variables) args)
                | _                         -> failwith "Invalid or unmatched pattern."
            
            let rec applyPattern (pattern : Pattern) expression variables =
                match (pattern.Left, expression) with
                | (p,                       e) when p = e -> (true, patternReplace pattern.Right variables)
                | (Template Anything,       _)            -> (true, patternReplace pattern.Right variables)
                | (Template (Variable var), e)            ->
                    variables.[var] <- e
                    (true, patternReplace pattern.Right variables)
                | (Function (f1, patternArgs),
                    Function (f2, funcArgs)) when f1 = f2 ->
                        let results = Seq.map executor.CalculateSymbolic funcArgs
                        if Seq.forall (fun res -> fst res) results
                        then (true, Function (f1, results |> Seq.map snd))
                        else (false, Function (f1, funcArgs))
                | (_,                       _)            -> (false, expression)
            
            let result = 
                simplificationPatterns
                |> Seq.map (fun p -> applyPattern p expression (new Dictionary<string, ExecutionTree> ()))
                |> Seq.tryFind (fun res -> fst res)
            match result with
            | Some e -> e
            | None   -> (false, expression)
            
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
