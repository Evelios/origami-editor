namespace Gui

open System
open Utilities.Extensions

module Color =
    let withAlpha (alpha: float) (color: String) : String =
        let alphaHex =
            (Float.remapToInt (0., 1.) (0, 255) alpha)
                .ToString("X2")

        "#" + alphaHex + color.[1..color.Length - 1]
