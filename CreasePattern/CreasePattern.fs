namespace CreasePattern

open FSharp.FGL
open Fold
open Geometry

type Label = int

type CreasePattern =
    CreasePattern of
        {| bounds: BoundingBox2D
           graph: Graph<Point2D, Label, EdgeAssignment> |}


module CreasePattern =
    open FSharp.FGL.Directed
    open Utilities.Collections

    (* Create a crease pattern using the "Unitless" units and a square sheet of paper. The crease pattern is created in
     * the square sheet of paper using cartesian coordinates. The bottom left corner is the origin (0, 0) and the paper
     * has a width and height of 1.
     *)
    let empty : CreasePattern =
        CreasePattern
            {| bounds = BoundingBox2D.empty
               graph = Graph.empty |}

    (* Accessors *)

    let private asEdge (start, finish, assignment) =
        Edge.betweenWithAssignment start finish assignment

    let size (CreasePattern creasePattern) =
        Size.create
            (creasePattern.bounds.maxX
             - creasePattern.bounds.minX)
            (creasePattern.bounds.maxY
             - creasePattern.bounds.minY)

    let edges (CreasePattern creasePattern) =
        List.map asEdge (Undirected.Edges.toEdgeList creasePattern.graph)

    let vertices (CreasePattern creasePattern) : Point2D list =
        List.map fst (Vertices.toVertexList creasePattern.graph)


    (* Modifiers *)

    let addVertices (vertices: Point2D list) (CreasePattern creasePattern: CreasePattern) : CreasePattern =
        let newVertices =
            List.filter (fun v -> not <| Vertices.contains v creasePattern.graph) vertices

        let graphVertices =
            List.map (fun vertex -> (vertex, vertex.GetHashCode())) newVertices

        let newBounds =
            List.fold (fun b v -> BoundingBox2D.containingPoint v b) creasePattern.bounds newVertices

        CreasePattern
            {| creasePattern with
                   bounds = newBounds
                   graph =
                       creasePattern.graph
                       |> Vertices.addMany graphVertices |}

    let addEdges (edges: Edge list) (CreasePattern cpData as creasePattern) : CreasePattern =
        let vertices : Point2D list =
            List.fold
                (fun (acc: Point2D list) (edge: Edge) ->
                    acc
                    @ [ edge.crease.start
                        edge.crease.finish ])
                []
                edges

        let creasePatternWithVertices = addVertices vertices creasePattern

        let graphEdges =
            List.map (fun (Edge edge) -> (edge.line.start, edge.line.finish, edge.assignment)) edges

        match creasePatternWithVertices with
        | CreasePattern cpVertData ->
            try
                CreasePattern
                    {| cpVertData with
                           graph =
                               cpVertData.graph
                               |> Undirected.Edges.addMany graphEdges |}
            with _ -> creasePattern


    /// Try adding an edge to the crease pattern. If the edge already exists, the same crease pattern will be returned
    let addEdge (edge: Edge) (creasePattern: CreasePattern) : CreasePattern = addEdges [ edge ] creasePattern

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
            [ Edge.betweenWithAssignment corners.tl corners.tr EdgeAssignment.Boundary
              Edge.betweenWithAssignment corners.tr corners.br EdgeAssignment.Boundary
              Edge.betweenWithAssignment corners.br corners.bl EdgeAssignment.Boundary
              Edge.betweenWithAssignment corners.bl corners.tl EdgeAssignment.Boundary ]

        empty
        |> addVertices vertices
        |> addEdges boundaries

    (* Queries *)

    // This function is currently linear but can be sped up with quad-trees
    // Get the closest vertex that is withing a particular distance
    let pointWithin distance vertex (CreasePattern creasePattern) : Point2D option =
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

    // Get the closest edge that is withing a particular distance
    let edgeWithin (distance: float) (vertex: Point2D) (CreasePattern creasePattern) : Edge option =
        let defaultCase =
            (infinity, ((Point2D.xy infinity infinity), (Point2D.xy -infinity -infinity), EdgeAssignment.Unassigned))

        let closestDistance, closestEdge =
            Edges.toEdgeList creasePattern.graph
            |> List.fold
                (fun closestEdge nextEdge ->
                    match closestEdge with
                    | closestDistance, _ as closestEdge ->
                        match nextEdge with
                        | start, finish, _ as nextEdge ->
                            let nextDistance =
                                (LineSegment2D.from start finish)
                                |> LineSegment2D.distanceToPoint vertex

                            printfn $"{nextEdge}"
                            printfn $"{nextDistance}"

                            if nextDistance < closestDistance then
                                (nextDistance, nextEdge)
                            else
                                closestEdge)
                defaultCase

        printfn $"{closestDistance}"

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
                (fun (first, second) -> Edge.betweenWithAssignment coordinates.[first] coordinates.[second])
                edges.vertices
                edges.assignment

        empty |> addEdges graphEdges

    /// Add in the crease pattern values into the fold frame
    let addToFoldFrame (CreasePattern creasePattern: CreasePattern) (frame: Fold.Frame) : Fold.Frame =
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
