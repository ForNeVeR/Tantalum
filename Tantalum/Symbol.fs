namespace Tantalum
    open System

    type Symbol (s : string) = 
        member symbol.ToBinary () : double =
            Double.Parse s

        override symbol.ToString () = s

        override symbol.Equals obj =
            match obj with
            | :? Symbol as symbol2  -> s = symbol2.ToString ()
            | _                     -> false

        override symbol.GetHashCode () =
            s.GetHashCode ()
