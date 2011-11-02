namespace Tantalum
    open System.Linq

    type Executor () =
        member executor.AddUnaryFunction (func : Function) (applyFunctor : double -> double) = ()
        member executor.AddBinaryFunction (func : Function) (applyFunctor : double * double -> double) = ()

        member executor.AddSimplificationPattern (pattern : Pattern) = ()
        member executor.AddNormalizationPattern (pattern : Pattern) = ()

        member executor.CalculateSymbolic (expression: ExecutionTree) =
            expression

        member executor.CalculateBinary (expression: ExecutionTree) =
            match expression with
            | Constant (Double d)               -> d
            | Constant (Symbolic s)             -> s.ToBinary ()
            | Function (func, args)
                when func.Arity = args.Count () -> failwith "Function application still not implemented."
            | Function _                        -> failwith "Wrong number of arguments."