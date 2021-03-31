namespace Fold

open FSharp.Json

type Faces =
    { vertices: int list list option
      edges: int list list option
      orders: ((int (*face id*)  * int (*face id*)  * int (*order*)) list ) option}

module Faces =
    
    let Create a: Faces = a
    
    let Empty: Faces =
        { vertices = None
          edges = None
          orders = None }


    (* Json *)

    let private jsonConfig =
        JsonConfig.create (jsonFieldNaming = (+) "faces_", serializeNone = SerializeNone.Omit)

    let private jsonConfigUnformatted =
        JsonConfig.create (jsonFieldNaming = (+) "faces_", serializeNone = SerializeNone.Omit, unformatted = true)

    let ToJson (faces: Faces): string = Json.serializeEx jsonConfig faces

    let ToJsonUnformatted (edges: Faces): string =
        Json.serializeEx jsonConfigUnformatted edges

    let FromJson = Json.deserializeEx<Faces> jsonConfig
