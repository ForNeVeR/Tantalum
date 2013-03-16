namespace Tantalum

/// Type representing expression tree.
[<StructuralComparison; StructuralEquality>]
type Expression =
    | Function of Function * Expression list
    | Constant of Symbol
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
