namespace CreasePattern

open FSharp.FGL
open FSharp.FGL.Directed
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
                          minY = min bounds.minY (Vertex.x vertex)
                          maxY = max bounds.maxY (Vertex.x vertex) })
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
        |> addVertices [ corners.tl
                         corners.tr
                         corners.bl
                         corners.br ]
        |> addEdges boundaries

    (* Queries *)


    (* Deserialization *)

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
