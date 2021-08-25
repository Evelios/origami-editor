namespace Geometry

open System
open LanguagePrimitives

type Float() =
    static let mutable digitPrecision = 12

    static member DigitPrecision
        with get () = digitPrecision
        and set v = digitPrecision <- v

[<AutoOpen>]
module Internal =

    let Epsilon : float = 10. ** (float -Float.DigitPrecision)
    let internal almostEqual (a: float) (b: float) : bool = abs (a - b) < FloatWithMeasure Epsilon
    let roundFloatTo (precision: int) (x: float) = Math.Round(x, precision)
    let roundFloat (x: float) = roundFloatTo Float.DigitPrecision x
