namespace Tantalum
    type ExecutionTree =
        | Function Function * ExecutionTree seq
        | Constant Constant

