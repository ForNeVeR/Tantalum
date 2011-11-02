namespace Tantalum
    type Executor () =
        member executor.AddUnaryFunction (func : Function) (applyFunctor : double -> double) = ()
        member executor.AddBinaryFunction (func : Function) (applyFunctor : double * double -> double) = ()

        member executor.AddSimplificationPattern (pattern : Pattern) = ()
        member executor.AddNormalizationPattern (pattern : Pattern) = ()

        member executor.CalculateSymbolic (expression: ExecutionTree) = ()
        member executor.CalculateBinary (expression: ExecutionTree) = ()