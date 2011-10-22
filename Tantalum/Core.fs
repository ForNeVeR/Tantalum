/// Tantalum core module.
module Tantalum.Core
    open System

    type Constant =
        | Symbol of string
        | Double of double

        override constant.ToString () =
            match constant with
            | Symbol s -> s
            | Double d -> String.Format ("{0}d", d)

    type BinaryOperation =
        | Addition
        | Substraction

        override operation.ToString () =
            match operation with
            | Addition     -> "+"
            | Substraction -> "-"
    
    type Node = 
        | Operation of BinaryOperation * Node * Node
        | Constant of Constant

        override node.ToString () =
            match node with
            | Operation (op, a, b) -> String.Format ("({0} {1} {2})", a, op, b)
            | Constant c           -> c.ToString ()

    let apply op =
        match op with
        | Addition     -> (+)
        | Substraction -> (-)

    let rec calculate node : double =
        match node with
        | Operation (operation, node1, node2) -> apply operation (calculate node1) (calculate node2)
        | Constant (Symbol symbol)            -> Convert.ToDouble symbol
        | Constant (Double double)            -> double

    let simplify node : Node =
        node
