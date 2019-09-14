namespace Tantalum.Test

open NUnit.Framework

open Tantalum

[<TestFixture>]
type SymbolTest() =
    [<Test>]
    member this.CreateTest() =
        let s = Symbol.Create "1.01"
        Assert.AreEqual(101I, s.Mantissa) |> ignore
        Assert.AreEqual(-2, s.Power) |> ignore

    [<Test>]
    member this.CreateNegativeTest() =
        let s = Symbol.Create "-1"
        Assert.AreEqual(-1I, s.Mantissa)
        Assert.AreEqual(0, s.Power)

    [<Test>]
    member this.SumTest() =
        let x = Symbol.Create "2.04"
        let y = Symbol.Create "7.99"
        let sum = x + y
        Assert.AreEqual(Symbol.Create "10.03", sum)

    [<Test>]
    member this.SubtractionTest() =
        let x = Symbol.Create "10"
        let y = Symbol.Create "14.00000001"
        let subtract = x - y
        Assert.AreEqual(Symbol.Create "-4.00000001", subtract)