namespace Fold

open FSharp.Json

type EdgeAssignment =
    | [<JsonUnionCase("B")>] Boundary
    | [<JsonUnionCase("M")>] Mountain
    | [<JsonUnionCase("V")>] Valley
    | [<JsonUnionCase("F")>] Flat
    | [<JsonUnionCase("U")>] Unassigned

type Edges =
    { Vertices: (int * int) list
      Faces: (int * int option) list
      Assignment: EdgeAssignment list
      FoldAngle: float list // should be angle
      Length: float list
      Orders: (int (*edge id*)  * int (*edge id*)  * int (*order*) ) list }

module Edges =

    let create a : Edges = a

    let empty : Edges =
        { Vertices = []
          Faces = []
          Assignment = []
          FoldAngle = []
          Length = []
          Orders = [] }
