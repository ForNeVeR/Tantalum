namespace Tantalum

/// Type representing function.
type Function =
    {
        /// Function string identifier. E.g. for addition it may be "+".
        Id : string;

        /// Function arity (number of arguments).
        Arity : int
    }
