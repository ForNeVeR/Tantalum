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

/// Type representing expression tree.
type ExecutionTree =
    | Function of Function * ExecutionTree list
    | Constant of Constant
    | Template of Template

    /// Converts object to string.
    override tree.ToString () : string =
        match tree with
        | Constant c -> c.ToString ()
        | Function (f, args) ->
            match f.Arity with
            | 2 ->
                let arg1 = Seq.nth 0 args
                let arg2 = Seq.nth 1 args
                sprintf "(%s %s %s)" (arg1.ToString ()) f.Id (arg2.ToString ())
            | _ -> 
                let buffer = ref (sprintf "%s" f.Id)
                args
                |> List.iter (fun arg -> buffer := sprintf "%s %s" !buffer (arg.ToString ()))
                !buffer
        | Template _ -> "Template"
