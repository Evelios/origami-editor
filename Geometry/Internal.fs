namespace Geometry

open LanguagePrimitives

[<AutoOpen>]
module Internal =

    [<Literal>]
    let Epsilon : float = 1e-8

    let internal almostEqual (a: float) (b: float) : bool = abs (a - b) < FloatWithMeasure Epsilon
