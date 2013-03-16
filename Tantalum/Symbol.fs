(* Copyright (C) 2011-2012 by ForNeVeR

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

namespace Tantalum

open System
open System.Text.RegularExpressions

/// Type for symbolic number as opposed to "binary" number.
[<StructuralEquality; StructuralComparison>]
type Symbol (mantissa : bigint, power : int) =
    struct
        static member private Normalize (mantissa : bigint) power =
            let rec normalize mantissa power =
                if mantissa % 10I = 0I && not mantissa.IsZero then
                    normalize (mantissa / 10I) (power + 1)
                else
                    mantissa, power
            normalize mantissa power
                   
        static member Create (s : string) =
            let parse s =
                let parseRegex = new Regex (@"(?<integer>[+-]?\d+)" +
                                            @"(\.(?<rational>\d+))?" +
                                            "(" +
                                                "[eE]" +
                                                @"(?<exponent>[+-]?\d+)" +
                                            ")?")
                let groups = (parseRegex.Match s).Groups
                let integer = groups.["integer"].Value
                let rational = groups.["rational"].Value
                let exponent = groups.["exponent"].Value
                let power = (if exponent <> "" then Convert.ToInt32 exponent else 0) - rational.Length
                let mantissa = bigint.Parse (integer + rational)
                mantissa, power

            let preMantissa, prePower = parse s
            let mantissa, power = Symbol.Normalize preMantissa prePower
            new Symbol (mantissa, power)

        static member Zero =
            Symbol.Create "0"

        member symbol.Power = power
        member symbol.Mantissa = mantissa

        static member (+) (a : Symbol, b : Symbol) =
            let rec pow a b =
                if b = 0 then
                    1I
                else
                    a * pow a (b - 1)
                    
            let minPower = min a.Power b.Power
            let mantissaA = a.Mantissa * pow 10I (a.Power - minPower)
            let mantissaB = b.Mantissa * pow 10I (b.Power - minPower)
            let sumMantissa = mantissaA + mantissaB
            let mantissa, power = Symbol.Normalize sumMantissa minPower
            new Symbol (mantissa, power)

        static member (-) (a : Symbol, b : Symbol) =
            let rec pow a b =
                if b = 0 then
                    1I
                else
                    a * pow a (b - 1)
                    
            let minPower = min a.Power b.Power
            let mantissaA = a.Mantissa * pow 10I (a.Power - minPower)
            let mantissaB = b.Mantissa * pow 10I (b.Power - minPower)
            let sumMantissa = mantissaA - mantissaB
            let mantissa, power = Symbol.Normalize sumMantissa minPower
            new Symbol (mantissa, power)

        /// Converts symbol to its binary representation.
        member symbol.ToBinary () : double =
            let doubleMantissa = Convert.ToDouble (symbol.Mantissa.ToString ())
            doubleMantissa * Math.Pow (10.0, Convert.ToDouble(symbol.Power))

        /// Converts an object to a string.
        override symbol.ToString () : string =
            (symbol.ToBinary ()).ToString ()
    end
