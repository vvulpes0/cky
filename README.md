# CKY Parsing in Haskell

This is a basic implementation of a CKY parser
that supports grammars with and without weights.
The sample grammar in `test.lhs` is particularly small,
but it may serve as a useful example.

For best results in probabilistic grammars,
consider importing `Data.Ratio` and using `(Weighted Rational)`
to ensure there is no loss of precision.
Future work may include implementing a (lossy) alternative
based on negative logs in order to replace multiplication with addition.
