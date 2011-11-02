namespace Tantalum
    open System

    type Symbol (s : string) = 
        member symbol.ToBinary () : double =
            Double.Parse s

        override symbol.ToString () = s
