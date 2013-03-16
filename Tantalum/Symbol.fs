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
