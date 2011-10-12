module Calculation
open System

type DoubleConstant = { Value : float }

type SymbolicConstant = { Value : string }

let convert (symbol : string) =
     Double.Parse(symbol)

type Constant =
    | Symbolic of SymbolicConstant
    | Double of DoubleConstant

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

let rec calculate node =
    match node with
    | Operation (operation, node1, node2) -> apply operation (calculate node1) (calculate node2)
    | Constant (Symbolic symbolic)        -> convert symbolic.Value
    | Constant (Double double)            -> double.Value
