namespace Tantalum

/// Type for simplification / normalization pattern.
type Pattern =
    {
        /// Left part of pattern (left part of equality operator).
        Left : Expression;

        /// Right part of pattern (right part of equality operator).
        Right : Expression
    }
