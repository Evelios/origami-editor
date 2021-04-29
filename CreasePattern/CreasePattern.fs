namespace CreasePattern

open FSharp.FGL


type Edge =
    | Boundary
    | Mountain
    | Valley

type Label = None

type LengthUnit =
    | Unitless
    | Pixels
    static member all = [ Unitless; Pixels ]

type CreasePattern =
    { units: LengthUnit
      graph: Graph<Vector, Label, Edge> }


module CreasePattern =
    (* Create a crease pattern using the "Unitless" units and a square sheet of paper. The crease pattern is created in
     * the square sheet of paper using cartesian coordinates. The bottom left corner is the origin (0, 0) and the paper
     * has a width and height of 1.
     *)
    let create : CreasePattern =
        { units = LengthUnit.Unitless
          graph = Graph.empty }

    //              let vertices =
//                  {| bottomLeft = Vector.in2D 0. 0.
//                     topLeft = Vector.in2D 1. 0.
//                     topRight = Vector.in2D 1. 1.
//                     bottomRight = Vector.in2D 0. 1. |}
//
//              Graph.empty
//              |> Undirected.Vertices.addMany
//                  (List.map (fun v -> (v, None))
//                       [ vertices.bottomLeft
//                         vertices.topLeft
//                         vertices.topRight
//                         vertices.bottomRight ])
//              |> Undirected.Edges.addMany [ (vertices.bottomLeft, vertices.topLeft, Edge.Boundary)
//                                            (vertices.topLeft, vertices.topRight, Edge.Boundary)
//                                            (vertices.topRight, vertices.bottomRight, Edge.Boundary)
//                                            (vertices.bottomRight, vertices.bottomLeft, Edge.Boundary) ] }



    let pointWithin distance creasePattern = ()
