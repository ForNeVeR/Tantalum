﻿/// Module containing standard operations definitions.
module Tantalum.StandardOperations
open System

/// Registers standard operations for use in executor.
let public Register (executor : IExecutor) =
    executor.AddUnaryFunction {Id = "+"; Arity = 1} <| fun a -> a
    executor.AddUnaryFunction {Id = "-"; Arity = 1} <| fun a -> -a
    
    executor.AddBinaryFunction {Id = "+"; Arity = 2} <| fun (a, b) -> a + b
    executor.AddBinaryFunction {Id = "-"; Arity = 2} <| fun (a, b) -> a - b
    executor.AddBinaryFunction {Id = "*"; Arity = 2} <| fun (a, b) -> a * b
    executor.AddBinaryFunction {Id = "/"; Arity = 2} <| fun (a, b) -> a / b
    executor.AddBinaryFunction {Id = "^"; Arity = 2} <| Math.Pow

    let simplificationPatterns = [
        // + a = a
        {Left = Function ({Id = "+"; Arity = 1},
                          [Template <| Variable "a"]);
         Right = Template <| Variable "a"};

        // - (- a) = a
        {Left = Function ({Id = "-"; Arity = 1},
                          [Function ({Id = "-"; Arity = 1},
                                     [Template <| Variable "a"])]);
         Right = Template <| Variable "a"};

        // a + 0 = a
        {Left = Function ({Id = "+"; Arity = 2},
                          [Template <| Variable "a";
                           Constant Symbol.Zero]);
         Right = Template <| Variable "a"};

        // a * 0 = 0
        {Left = Function ({Id = "*"; Arity = 2},
                          [Template Anything;
                           Constant Symbol.Zero]);
         Right = Constant Symbol.Zero};

         // 0 / a = 0
         {Left = Function ({Id = "/"; Arity = 2},
                           [Constant Symbol.Zero;
                            Template Anything]);
          Right = Constant Symbol.Zero}
    ]

    let normalizationPatterns = [
        // a + b = b + a
        {Left = Function ({Id = "+"; Arity = 2},
                          [Template <| Variable "a";
                           Template <| Variable "b"]);
         Right = Function ({Id = "+"; Arity = 2},
                           [Template <| Variable "b";
                            Template <| Variable "a"])}

        // a * b = b * a
        {Left = Function ({Id = "*"; Arity = 2},
                          [Template <| Variable "a";
                           Template <| Variable "b"]);
         Right = Function ({Id = "*"; Arity = 2},
                           [Template <| Variable "b";
                            Template <| Variable "a"])}
    ]

    simplificationPatterns
    |> List.iter executor.AddSimplificationPattern

    normalizationPatterns
    |> List.iter executor.AddNormalizationPattern