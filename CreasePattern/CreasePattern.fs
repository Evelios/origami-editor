namespace CreasePattern

open FSharp.FGL
open Fold
open Geometry

type Label = int

type Bounds =
    { minX: float
      maxX: float
      maxY: float
      minY: float }

type CreasePattern =
    { bounds: Bounds
      graph: Graph<Point2D, Label, EdgeAssignment> }


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

    let private asEdge (start, finish, assignment) =
        Edge.create
            { start = start
              finish = finish
              assignment = assignment }

    let size creasePattern =
        Size.create
            (creasePattern.bounds.maxX
             - creasePattern.bounds.minX)
            (creasePattern.bounds.maxY
             - creasePattern.bounds.minY)

    let edges creasePattern : Edge list =
        List.map asEdge (Undirected.Edges.toEdgeList creasePattern.graph)

    let vertices creasePattern : Point2D list =
        List.map fst (Vertices.toVertexList creasePattern.graph)


    (* Modifiers *)

    let addVertices (vertices: Point2D list) (creasePattern: CreasePattern) : CreasePattern =
        let newVertices =
            List.filter (fun v -> not <| Vertices.contains v creasePattern.graph) vertices

        let graphVertices =
            List.map (fun vertex -> (vertex, vertex.GetHashCode())) newVertices

        let newBounds =
            List.fold
                (fun bounds (vertex: Point2D ) ->
                    { bounds with
                          minX = min bounds.minX vertex.x
                          maxX = max bounds.maxX vertex.x
                          minY = min bounds.minY vertex.y
                          maxY = max bounds.maxY vertex.y })
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

    /// Try adding an edge to the crease pattern. If the edge already exists, the same edge will be returned
    let addEdge (edge: Edge) (creasePattern: CreasePattern) : CreasePattern =
        let vertices = [ edge.start; edge.finish ]
        let creasePatternWithVertices = addVertices vertices creasePattern

        let graphEdge =
            (edge.start, edge.finish, edge.assignment)

        let graph =
            try
                creasePatternWithVertices.graph
                |> Undirected.Edges.add graphEdge
            with _ -> creasePattern.graph

        { creasePatternWithVertices with
              graph = graph }

    (* Builder *)

    ///  Create a basic crease pattern within a 0 -> 1 grid system with edge boundaries
    let create : CreasePattern =
        let corners =
            {| tl = (Point2D.xy 0. 1.)
               tr = (Point2D.xy 1. 1.)
               br = (Point2D.xy 1. 0.)
               bl = (Point2D.xy 0. 0.) |}

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

    /// This function is currently linear but can be sped up with quad-trees
    let pointWithin distance vertex creasePattern : Point2D option =
        let distanceSquared = distance * distance
        let distSquaredToVertex = Point2D.distanceSquaredTo vertex

        let vertexDistance, closestVertex =
            Vertices.toVertexList creasePattern.graph
            |> List.fold
                (fun (distanceSquared, closestVertex) (nextVertex, _) ->
                    let nextDistanceSquared = distSquaredToVertex nextVertex

                    if nextDistanceSquared < distanceSquared then
                        (nextDistanceSquared, nextVertex)

                    else
                        (distanceSquared, closestVertex))
                (infinity, Point2D.xy infinity infinity)

        if vertexDistance < distanceSquared then
            Some closestVertex
        else
            None

    let edgeWithin distance vertex creasePattern =
        let defaultCase =
            (infinity, ((Point2D.xy infinity infinity), (Point2D.xy -infinity -infinity), EdgeAssignment.Unassigned))

        let closestDistance, closestEdge =
            Edges.toEdgeList creasePattern.graph
            |> List.fold
                (fun (closestDistance, closestEdge) nextEdge ->
                    match nextEdge with
                    | start, finish, _ as nextEdge ->
                        let nextDistance =
                            LineSegment2D.distanceToVertex vertex (LineSegment2D.fromTo start finish)

                        if nextDistance < closestDistance then
                            (nextDistance, nextEdge)

                        else
                            (closestDistance, closestEdge))
                defaultCase

        if closestDistance < distance then
            Some(asEdge closestEdge)
        else
            None


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
        let vertexLookup : Map<Point2D, int> =
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

        let edgeVertices : (Point2D * Point2D) list =
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
