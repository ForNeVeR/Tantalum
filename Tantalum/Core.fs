/// Tantalum core module.
module Tantalum.Core
    open System

    /// Type for binary values.
    type Binary = double

    /// Type for symbolic values.
    type Symbol = string

    /// Binary zero constant.
    let BinaryZero = 0.0

    /// Symbolic zero constant.
    let SymbolicZero = "0"

    /// Converts symbolic value to binary value.
    let Convert (symbol : Symbol) : Binary =
        Convert.ToDouble symbol

    /// Symbolic expression type. Will be computed with possible accuracy loss only on demand.
    [<CustomEquality; NoComparison>]
    type Expression =
        | BinaryConstant   of Binary
        | SymbolicConstant of Symbol
        | UnaryOperation   of string
                            * (Binary -> Binary)
                            * (Expression -> Expression)
                            * Expression
        | BinaryOperator   of string
                            * (Binary -> Binary -> Binary)
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
                | BinaryConstant value ->
                    match otherExpression with
                     | BinaryConstant otherValue when value = otherValue -> true
                     | _                                                 -> false
                | SymbolicConstant value ->
                    match otherExpression with
                     | SymbolicConstant otherValue when value = otherValue -> true
                     | _                                                   -> false
                | _ -> false
            | _ -> false

    /// Simplification function. Never causes any accuracy loss in expression.
    let rec Simplify expression =
        match expression with
        | UnaryOperation (_, _, simplification, arg)          -> simplification (Simplify arg)
        | BinaryOperator (_, _, simplification, (arg1, arg2)) -> simplification (Simplify arg1) (Simplify arg2)
        | other                                               -> other

    /// Calculates expression to binary number format. Can cause accuracy loss.
    let rec Execute expression =
        match expression with
        | BinaryConstant   value                       -> value
        | SymbolicConstant symbol                      -> Convert symbol
        | UnaryOperation   (_, apply, _, arg)          -> apply (Execute arg)
        | BinaryOperator   (_, apply, _, (arg1, arg2)) -> apply (Execute arg1) (Execute arg2)
