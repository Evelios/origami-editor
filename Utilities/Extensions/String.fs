namespace Utilities.Extensions

open System

module String =

    /// Try parsing a string into a floating point number. Will return None on failure
    let parseFloat (s: string) : float option =
        try
            Some(float s)
        with
        | _ -> None

    let strip (stripChars: string) (text: string) : string =
        text.Split(stripChars.ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
        |> String.Concat
