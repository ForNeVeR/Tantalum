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
            expression

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