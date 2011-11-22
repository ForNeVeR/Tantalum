(* Copyright (C) 2011 by ForNeVeR

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE. *)

/// Module containing standard operations definitions.
module Tantalum.StandardOperations

open System

/// Registers standard operations for use in executor.
let public Register (executor : IExecutor) =
    executor.AddUnaryFunction {Id = "+"; Arity = 1} (fun a -> Constant a) (fun a -> a)
    executor.AddUnaryFunction {Id = "-"; Arity = 1} (fun a -> 
        Function ({Id = "-"; Arity = 1}, [Constant a])) (fun a -> -a)
    
    executor.AddBinaryFunction {Id = "+"; Arity = 2} (fun (a, b) -> Constant (a + b)) (fun (a, b) -> a + b)
    executor.AddBinaryFunction {Id = "-"; Arity = 2} (fun (a, b) -> Constant (a - b)) (fun (a, b) -> a - b)
    executor.AddBinaryFunction {Id = "*"; Arity = 2} (fun (a, b) -> 
    Function ({Id = "*"; Arity = 2}, [Constant a; Constant b])) (fun (a, b) -> a * b)
    executor.AddBinaryFunction {Id = "/"; Arity = 2} (fun (a, b) ->
        Function ({Id = "/"; Arity = 2}, [Constant a; Constant b])) (fun (a, b) -> a / b)
    executor.AddBinaryFunction {Id = "^"; Arity = 2} (fun (a, b) ->
        Function ({Id = "^"; Arity = 2}, [Constant a; Constant b])) Math.Pow

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

         // a * 1 = a
         {Left = Function ({Id = "*"; Arity = 2},
                          [Template <| Variable "a";
                           Constant <| Symbol.Create "1"]);
         Right = Template <| Variable "a"};

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
