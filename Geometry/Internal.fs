namespace Geometry

open LanguagePrimitives

[<AutoOpen>]
module Internal =
    [<Literal>]
    let DigitPrecision : int = 8

    let Epsilon : float = 10. ** (float -DigitPrecision)
    let ExtraEpsilon : float = 10. ** (float -(DigitPrecision - 1))
    let internal almostEqual (a: float) (b: float) : bool = abs (a - b) < FloatWithMeasure Epsilon