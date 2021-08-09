namespace Fold

open FSharp.Json

type EdgeAssignment =
    | [<JsonUnionCase("B")>] Boundary
    | [<JsonUnionCase("M")>] Mountain
    | [<JsonUnionCase("V")>] Valley
    | [<JsonUnionCase("F")>] Flat
    | [<JsonUnionCase("U")>] Unassigned

type Edges =
    { vertices: (int * int) list
      faces: (int * int option) list
      assignment: EdgeAssignment list
      foldAngle: float list // should be angle
      length: float list
      orders: (int (*edge id*)  * int (*edge id*)  * int (*order*) ) list }

module Edges =
    
    let create a: Edges = a
    
    let empty: Edges =
        { vertices = []
          faces = []
          assignment = []
          foldAngle = []
          length = []
          orders = [] }
