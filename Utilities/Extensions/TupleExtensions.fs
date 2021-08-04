namespace Utilities.Extensions

module Tuple2 =
    let replicate x = x, x

    let curry f x y = f (x, y)

    let uncurry f (x, y) = f x y

    let swap (x, y) = (y, x)

    let mapFst f (x, y) = f x, y

    let mapSnd f (x, y) = x, f y

    let extendFst f (x, y) = f (x, y), y

    let extendSnd f (x, y) = x, f (x, y)

    let optionOfFst f (x, y) =
        match f x with
        | Some x' -> Some(x', y)
        | None -> None

    let optionOfSnd f (x, y) =
        match f y with
        | Some y' -> Some(x, y')
        | None -> None
