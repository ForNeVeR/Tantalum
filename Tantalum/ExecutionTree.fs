namespace Tantalum
    type ExecutionTree =
        | Function of Function * ExecutionTree seq
        | Constant of Constant
        | Template of Template
