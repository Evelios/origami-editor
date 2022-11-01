namespace Gui



module FileLoader =
    open System
    open System.IO
    open FSharp.Json
    open FSharp.Core.Extra

    open Origami.Fold

    type Error =
        | OsError of exn
        | DeserializationError of string

    let readFile path : Result<string seq, Error> =

        try
            seq {
                use reader =
                    new StreamReader(File.OpenRead(path))

                while not reader.EndOfStream do
                    yield reader.ReadLine()
            }
            |> Ok
        with
        | error -> Error <| OsError error

    /// TODO: catch json deserialization errors
    let loadFoldFile (path: string) : Result<Fold<'Coordinates>, Error> =
        readFile path
        |> Result.bind (
            String.concat Environment.NewLine
            >> Fold.fromJson
            >> Result.bindError (DeserializationError >> Error)
        )
