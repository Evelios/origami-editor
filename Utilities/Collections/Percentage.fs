namespace Utilities.Collections

[<Measure>]
type fraction

[<Measure>]
type ratio

type Percentage = private Ratio of float<ratio>

module Percentage =
    let private fractionOfRatio = 100.<fraction/ratio>

    (* Builders *)
    let ofFraction fraction = Ratio(fraction / fractionOfRatio)

    let ofRatio ratio = Ratio ratio
    let ofFloat (ratio: float) = Ratio(ratio * 1.<_>)

    (* Accessors *)
    let fraction (Ratio ratio) = ratio * fractionOfRatio

    let ratio (Ratio ratio) = ratio
    let float (Ratio ratio) = float ratio
