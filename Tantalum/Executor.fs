namespace Tantalum
    type Executor =
        member executor.AddUnaryFunction (func : Function) (applyFunctor : double -> double) = ()
        member executor.AddBinaryFunction (func : Function) (applyFunctor : double * double -> double) = ()

        member executor.AddPattern (pattern : Pattern) = ()

        member executor.CalculateSymbolic (expression: ExecutionTree) = ()
        member executor.CalculateBinary (expression: ExecutionTree) = ()