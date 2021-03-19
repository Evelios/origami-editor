namespace Fold

open FSharp.Json

module FoldModule =

    type Fold = { file: File }

    let create file: Fold = file

    let toJson (fold: Fold): string = Json.serialize fold

    let fromJson = Json.deserialize<Fold>
