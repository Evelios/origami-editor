namespace Fold

open FSharp.Json

type EdgeAssignment =
    | [<JsonUnionCase("boundary")>] Boundary
    | [<JsonUnionCase("mountain")>] Mountain
    | [<JsonUnionCase("valley")>] Valley
    | [<JsonUnionCase("flat")>] Flat
    | [<JsonUnionCase("unassigned")>] Unassigned

type Edges =
    { vertices: ((int * int) list) option
      faces: ((int * int * int * int) list) option
      assignment: EdgeAssignment list option
      foldAngle: float list option // should be angle
      length: float list option
      orders: ((int (*edge id*)  * int (*edge id*)  * int (*order*) ) list) option }

module Edges =
    let Empty: Edges =
        { vertices = None
          faces = None
          assignment = None
          foldAngle = None
          length = None
          orders = None }


    (* Json *)

    let private jsonConfig =
        JsonConfig.create (jsonFieldNaming = (+) "edges_", serializeNone = SerializeNone.Omit)

    let private jsonConfigUnformatted =
        JsonConfig.create (jsonFieldNaming = (+) "edges_", serializeNone = SerializeNone.Omit, unformatted = true)

    let ToJson (edges: Edges): string = Json.serializeEx jsonConfig edges

    let ToJsonUnformatted (edges: Edges): string =
        Json.serializeEx jsonConfigUnformatted edges

    let FromJson = Json.deserializeEx<Edges> jsonConfig
