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

namespace Tantalum

open System

/// Type for symbolic number as opposed to "binary" number.
type Symbol (s : string) = 
    /// Converts symbol to its binary representation.
    member symbol.ToBinary () : double =
        Double.Parse s

    /// Converts object to string.
    override symbol.ToString () : string =
        s

    /// Equates two symbol instances.
    override symbol.Equals (obj : obj) : bool =
        match obj with
        | :? Symbol as symbol2  -> s = symbol2.ToString ()
        | _                     -> false

    /// Calculates hashcode for symbol.
    override symbol.GetHashCode () : int =
        s.GetHashCode ()
