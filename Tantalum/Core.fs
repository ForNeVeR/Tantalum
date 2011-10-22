/// Tantalum core module.
module Tantalum.Core
    open System

    let convert (symbol : string) =
         Double.Parse(symbol)

    type Constant =
        | Symbol of string
        | Double of double

    type BinaryOperation =
        | Addition
        | Substraction
    
    type Node = 
        | Operation of BinaryOperation * Node * Node
        | Constant of Constant

    let apply op =
        match op with
        | Addition     -> (+)
        | Substraction -> (-)

    let rec calculate node : double =
        match node with
        | Operation (operation, node1, node2) -> apply operation (calculate node1) (calculate node2)
        | Constant (Symbol symbol)            -> convert symbol
        | Constant (Double double)            -> double
