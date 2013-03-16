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

    executor.AddUnaryFunction {Id = "sin"; Arity = 1} (fun a -> Function ({Id = "sin"; Arity = 1},
                                                        [Constant a])) (fun a -> Math.Sin a)
    executor.AddUnaryFunction {Id = "cos"; Arity = 1} (fun a -> Function ({Id = "cos"; Arity = 1},
                                                        [Constant a])) (fun a -> Math.Cos a)
    executor.AddUnaryFunction {Id = "tg"; Arity = 1} (fun a -> Function ({Id = "tg"; Arity = 1},
                                                        [Constant a])) (fun a -> Math.Tan a)
    executor.AddUnaryFunction {Id = "ctg"; Arity = 1} (fun a -> Function ({Id = "ctg"; Arity = 1},
                                                        [Constant a])) (fun a -> 1.0 / Math.Tan a)

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
          Right = Constant Symbol.Zero};
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
                            Template <| Variable "a"])};

        // a / b = a * (1 / b)
        {Left = Function({Id = "/"; Arity = 2},
                         [Template (Variable "a");
                          Template (Variable "b")]);
         Right = Function({Id = "*"; Arity = 2},
                          [Template (Variable "a");
                           Function({Id = "/"; Arity = 2},
                                    [Constant (Symbol (bigint 1, 0));
                                     Template (Variable "b")])])};

        // (a + b) + c = a + (b + c)
        {Left = Function({Id = "+"; Arity = 2},
                         [Function({Id = "+"; Arity = 2},
                                   [Template (Variable "a");
                                    Template (Variable "b")]);
                          Template (Variable "c")]);
         Right = Function({Id = "+"; Arity = 2},
                          [Template (Variable "a");
                           Function({Id = "+"; Arity = 2},
                                    [Template (Variable "b");
                                     Template (Variable "c")])])};

        // (a * b) * c = a * (b * c)
        {Left = Function({Id = "*"; Arity = 2},
                         [Function({Id = "*"; Arity = 2},
                                   [Template (Variable "a");
                                    Template (Variable "b")]);
                          Template (Variable "c")]);
         Right = Function({Id = "*"; Arity = 2},
                          [Template (Variable "a");
                           Function({Id = "*"; Arity = 2},
                                    [Template (Variable "b");
                                     Template (Variable "c")])])}
    ]

    simplificationPatterns
    |> List.iter executor.AddSimplificationPattern

    normalizationPatterns
    |> List.iter executor.AddNormalizationPattern
