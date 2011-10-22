/// Tantalum core module.
module Tantalum.Core
    open System

    [<CustomEquality; NoComparison>]
    type Expression = 
        | BinaryConstant   of double
        | SymbolicConstant of string
        | UnaryOperation   of string
                            * (double -> double)
                            * (Expression -> Expression)
                            * Expression
        | BinaryOperator   of string
                            * (double -> double -> double)
                            * (Expression -> Expression -> Expression)
                            * (Expression * Expression)

        override expression.ToString () =
            match expression with
            | BinaryConstant   value                        -> sprintf "(%fb)" value
            | SymbolicConstant symbol                       -> sprintf "(%s)" symbol
            | UnaryOperation   (symbol, _, _, arg)          -> sprintf "(%s %s)" symbol (arg.ToString ())
            | BinaryOperator   (symbol, _, _, (arg1, arg2)) -> sprintf "(%s %s %s)" (arg1.ToString ()) symbol (arg2.ToString ())

        override expression.Equals other =
            match other with
            | :? Expression as otherExpression ->
                match expression with
                | BinaryConstant 0.0 ->
                     match otherExpression with
                     | SymbolicConstant "0" -> true
                     | BinaryConstant 0.0   -> true
                     | _                    -> false
                | SymbolicConstant "0" -> otherExpression.Equals expression
                | _                    -> false
            | _ -> false

    let rec simplify expression =
        match expression with
        | UnaryOperation (_, _, simplification, arg)          -> simplification (simplify arg)
        | BinaryOperator (_, _, simplification, (arg1, arg2)) -> simplification (simplify arg1) (simplify arg2)
        | other                                               -> other

    let rec execute expression =
        match expression with
        | BinaryConstant   value                       -> value
        | SymbolicConstant symbol                      -> Convert.ToDouble symbol
        | UnaryOperation   (_, apply, _, arg)          -> apply (execute arg)
        | BinaryOperator   (_, apply, _, (arg1, arg2)) -> apply (execute arg1) (execute arg2)

    let zero = SymbolicConstant "0"
