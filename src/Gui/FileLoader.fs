namespace Gui



module FileLoader =
    open System
    open System.IO
    open FSharp.Json

    open Fold

    type Error =
        | OsError of exn
        | DeserializationError of JsonDeserializationError

    let readFile path : Result<string seq, Error> =

        try
            seq {
                use reader = new StreamReader(File.OpenRead(path))

                while not reader.EndOfStream do
                    yield reader.ReadLine()
            }
            |> Ok
        with error -> Error <| OsError error

    /// TODO: catch json deserialization errors
    let loadFoldFile path : Result<Fold, Error> =
        readFile path
        |> Result.map (String.concat Environment.NewLine >> Fold.fromJson)
