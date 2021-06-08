namespace CreasePattern

open FSharp.FGL
open Fold

type Label = int

type Bounds =
    { minX: float
      maxX: float
      maxY: float
      minY: float }

type CreasePattern =
    { bounds: Bounds
      graph: Graph<Vertex, Label, EdgeAssignment> }


module CreasePattern =
    open FSharp.FGL.Directed
    open Utilities.Collections

    (* Create a crease pattern using the "Unitless" units and a square sheet of paper. The crease pattern is created in
     * the square sheet of paper using cartesian coordinates. The bottom left corner is the origin (0, 0) and the paper
     * has a width and height of 1.
     *)
    let empty : CreasePattern =
        { bounds =
              { minX = infinity
                maxX = -infinity
                minY = infinity
                maxY = -infinity }
          graph = Graph.empty }

    (* Accessors *)

    let size creasePattern =
        Size.create
            (creasePattern.bounds.maxX
             - creasePattern.bounds.minX)
            (creasePattern.bounds.maxY
             - creasePattern.bounds.minY)

    let edges creasePattern : Edge list =
        List.map
            (fun (start, finish, assignment) ->
                Edge.create
                    { start = start
                      finish = finish
                      assignment = assignment })
            (Undirected.Edges.toEdgeList creasePattern.graph)


    (* Modifiers *)

    let addVertices (vertices: Vertex list) (creasePattern: CreasePattern) : CreasePattern =
        let newVertices =
            List.filter (fun v -> not <| Vertices.contains v creasePattern.graph) vertices

        let graphVertices =
            List.map (fun vertex -> (vertex, Vertex.hashCode vertex)) newVertices

        let newBounds =
            List.fold
                (fun bounds vertex ->
                    { bounds with
                          minX = min bounds.minX (Vertex.x vertex)
                          maxX = max bounds.maxX (Vertex.x vertex)
                          minY = min bounds.minY (Vertex.y vertex)
                          maxY = max bounds.maxY (Vertex.y vertex) })
                creasePattern.bounds
                newVertices

        { creasePattern with
              bounds = newBounds
              graph =
                  creasePattern.graph
                  |> Vertices.addMany graphVertices }

    let addEdges (edges: Edge list) (creasePattern: CreasePattern) : CreasePattern =
        let vertices =
            List.fold (fun acc edge -> acc @ [ edge.finish; edge.start ]) [] edges

        let creasePatternWithVertices = addVertices vertices creasePattern

        let graphEdges =
            List.map (fun edge -> (edge.start, edge.finish, edge.assignment)) edges

        { creasePatternWithVertices with
              graph =
                  creasePatternWithVertices.graph
                  |> Undirected.Edges.addMany graphEdges }

    (* Builder *)

    ///  Create a basic crease pattern within a 0 -> 1 grid system with edge boundaries
    let create : CreasePattern =
        let corners =
            {| tl = (Vertex.in2d 0. 1.)
               tr = (Vertex.in2d 1. 1.)
               br = (Vertex.in2d 1. 0.)
               bl = (Vertex.in2d 0. 0.) |}

        let vertices =
            [ corners.tl
              corners.tr
              corners.bl
              corners.br ]

        let boundaries =
            [ Edge.create
                { start = corners.tl
                  finish = corners.tr
                  assignment = EdgeAssignment.Boundary }
              Edge.create
                  { start = corners.tr
                    finish = corners.br
                    assignment = EdgeAssignment.Boundary }
              Edge.create
                  { start = corners.br
                    finish = corners.bl
                    assignment = EdgeAssignment.Boundary }
              Edge.create
                  { start = corners.bl
                    finish = corners.tl
                    assignment = EdgeAssignment.Boundary } ]

        empty
        |> addVertices vertices
        |> addEdges boundaries

    (* Queries *)


    (* Serialization & Deserialization *)

    /// Convert the frame values in the fold file to a crease pattern
    let fromFoldValues (vertices: Fold.Vertices) (edges: Fold.Edges) (_: Fold.Faces) : CreasePattern =
        let coordinates = Array.ofList vertices.coords

        let graphEdges =
            List.map2
                (fun (first, second) assignment ->
                    Edge.create
                        { start = coordinates.[first]
                          finish = coordinates.[second]
                          assignment = assignment })
                edges.vertices
                edges.assignment

        empty |> addEdges graphEdges

    /// Add in the crease pattern values into the fold frame
    let addToFoldFrame (creasePattern: CreasePattern) (frame: Fold.Frame) : Fold.Frame =
        let vertexLookup : Map<Vertex, int> =
            Vertices.toVertexList creasePattern.graph
            |> List.indexed
            |> List.fold (fun map (id, (vert, _)) -> Map.add vert id map) Map.empty

        let vertices =
            Fold.Vertices.create
                { coords =
                      Vertices.toVertexList creasePattern.graph
                      |> List.map fst
                  vertices = []
                  faces = [] }

        let edges =
            Undirected.Edges.toEdgeList creasePattern.graph

        let edgeVertices : (Vertex * Vertex) list =
            List.map (fun (v1, v2, _) -> (v1, v2)) edges

        let edgeVerticesIndexed =
            List.map (fun (v1, v2) -> (Map.find v1 vertexLookup, Map.find v2 vertexLookup)) edgeVertices

        let edges =
            Fold.Edges.create
                { vertices = edgeVerticesIndexed
                  faces = []
                  assignment = List.map (fun (_, _, assignment) -> assignment) edges
                  foldAngle = []
                  length = []
                  orders = [] }

        frame
        |> Frame.setVertices vertices
        |> Frame.setEdges edges
