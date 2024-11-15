#nowarn "3535"

namespace AOC.FSharp.Common

open System.Numerics

[<Interface>]
type IVector<'V, 'U when 'V :> IVector<'V, 'U> and 'U :> INumber<'U>> =
    inherit IAdditionOperators<'V, 'V, 'V>
    inherit ISubtractionOperators<'V, 'V, 'V>
    inherit IUnaryPlusOperators<'V, 'V>
    inherit IUnaryNegationOperators<'V, 'V>
    inherit IMultiplyOperators<'V, 'V, 'V>
    inherit IDivisionOperators<'V, 'V, 'V>
    inherit IMultiplicativeIdentity<'V, 'V>
    inherit IAdditiveIdentity<'V, 'V>

    static abstract member one: 'V
    static abstract member zero: 'V
    static abstract member size: int
    abstract member Item: int -> 'U with get, set
