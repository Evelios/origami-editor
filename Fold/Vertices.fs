namespace Fold

open FSharp.Json

type Vertices =
    { coords: ((float * float) list) option
      vertices: int list option
      faces: int list list option }

module Vertices =
    let Empty: Vertices =
        { coords = None
          vertices = None
          faces = None }


    (* Json *)

    let private jsonConfig =
        JsonConfig.create (jsonFieldNaming = (+) "vertices_", serializeNone = SerializeNone.Omit)

    let private jsonConfigUnformatted =
        JsonConfig.create (jsonFieldNaming = (+) "vertices_", serializeNone = SerializeNone.Omit, unformatted = true)

    let ToJson (vertices: Vertices): string = Json.serializeEx jsonConfig vertices

    let ToJsonUnformatted (vertices: Vertices): string =
        Json.serializeEx jsonConfigUnformatted vertices

    let FromJson = Json.deserializeEx<Vertices> jsonConfig
