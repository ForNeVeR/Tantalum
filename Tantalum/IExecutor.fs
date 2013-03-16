namespace Tantalum

type IExecutor =
    abstract member AddUnaryFunction         : Function -> (Symbol -> Expression) -> (double -> double) -> unit
    abstract member AddBinaryFunction        : Function -> (Symbol * Symbol -> Expression) -> (double * double -> double) -> unit
    abstract member AddSimplificationPattern : Pattern -> unit
    abstract member AddNormalizationPattern  : Pattern -> unit
    abstract member CalculateSymbolic        : Expression -> Expression
    abstract member CalculateBinary          : Expression -> double
