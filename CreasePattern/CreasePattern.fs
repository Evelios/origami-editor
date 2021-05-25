namespace CreasePattern

open FSharp.FGL
open Fold

type Label = int

type CreasePattern =
    { graph: Graph<Vertex, Label, EdgeAssignment> }


module CreasePattern =
    (* Create a crease pattern using the "Unitless" units and a square sheet of paper. The crease pattern is created in
     * the square sheet of paper using cartesian coordinates. The bottom left corner is the origin (0, 0) and the paper
     * has a width and height of 1.
     *)
    let create : CreasePattern = { graph = Graph.empty }


    (* Modifiers *)

    let addVertices (vertices: Vertex list) (creasePattern: CreasePattern) : CreasePattern =
        let graphVertices =
            vertices
            |> List.filter (fun v -> not <| Vertices.contains v creasePattern.graph)
            |> List.map (fun vertex -> (vertex, Vertex.hashCode vertex))

        { creasePattern with
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


    (* Queries *)

    let pointWithin distance creasePattern = ()


    (* Deserialization *)

    let fromFoldValues (vertices: Fold.Vertices) (edges: Fold.Edges) (faces: Fold.Faces) : CreasePattern =
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

        create
        |> addEdges graphEdges
