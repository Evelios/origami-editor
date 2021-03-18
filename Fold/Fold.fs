namespace Fold

open FSharp.Json

type Version = { major: int; minor: int; patch: int }

type File =
    { spec: Version Option
      creator: string
      author: string
      title: string
      description: string }

type Fold = { file: File }

module Fold =

    let toJson (fold: Fold) = Json.serialize fold

    let fromJson = Json.deserialize<Fold>
