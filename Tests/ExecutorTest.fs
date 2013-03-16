namespace Tantalum.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Tantalum

[<TestClass>]
type ExecutorTest() =
    static let parser = Voice.createParser()
    static let executor = Executor.executor()
    static do StandardOperations.Register executor

    static let expression text =
        Voice.Parse parser text

    [<TestMethod>]
    member this.SimpleExpression() =
        let x = expression "2 + 2 * 2"
        let simplified = executor.CalculateSymbolic x
        let result = executor.CalculateBinary x
        let expected = expression "6"
        Assert.AreEqual(expected, simplified)
        Assert.AreEqual(6, result)
