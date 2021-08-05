namespace CreasePattern

open FSharp.FGL
open Geometry

type Label = int

type CreasePattern =
    | CreasePattern of
        {| bounds: BoundingBox2D
           graph: Graph<Point2D, Label, EdgeAssignment>
           unit: LengthUnit
           author: string
           title: string
           description: string |}

    member this.unit =
        match this with
        | CreasePattern cp -> cp.unit

    member this.author =
        match this with
        | CreasePattern cp -> cp.author

    member this.title =
        match this with
        | CreasePattern cp -> cp.title

    member this.description =
        match this with
        | CreasePattern cp -> cp.description


module CreasePattern =
    open FSharp.FGL.Directed

    (* Create a crease pattern using the "Unitless" units and a square sheet of paper. The crease pattern is created in
     * the square sheet of paper using cartesian coordinates. The bottom left corner is the origin (0, 0) and the paper
     * has a width and height of 1.
     *)
    let empty: CreasePattern =
        CreasePattern
            {| bounds = BoundingBox2D.empty
               graph = Graph.empty
               unit = Unitless
               author = ""
               title = ""
               description = "" |}

    (* Accessors *)

    let private asEdge (start, finish, assignment) =
        Edge.betweenWithAssignment start finish assignment

    let size (CreasePattern creasePattern) =
        Size.create
            (creasePattern.bounds.maxX
             - creasePattern.bounds.minX)
            (creasePattern.bounds.maxY
             - creasePattern.bounds.minY)

    let edges (CreasePattern creasePattern) : Edge list =
        Undirected.Edges.fold (fun edges v1 v2 e -> Edge.betweenWithAssignment v1 v2 e :: edges) [] creasePattern.graph
        // Undirected fold is returning the edges in both directions so we need to filter out half of them
        |> Seq.distinctBy
            (fun edge ->
                Set.ofList [ edge.line.start
                             edge.line.finish ])
        |> Seq.toList

    let vertices (CreasePattern creasePattern) : Point2D list =
        List.map fst (Vertices.tovertexList creasePattern.graph)

    let elements creasePattern : GraphElement list =
        List.map VertexElement (vertices creasePattern)
        @ List.map EdgeElement (edges creasePattern)



    (* Modifiers *)

    let setUnit unit (CreasePattern creasePattern) =
        CreasePattern {| creasePattern with unit = unit |}

    let setAuthor author (CreasePattern creasePattern) =
        CreasePattern {| creasePattern with author = author |}

    let setTitle title (CreasePattern creasePattern) =
        CreasePattern {| creasePattern with title = title |}

    let setDescription description (CreasePattern creasePattern) =
        CreasePattern
            {| creasePattern with
                   description = description |}

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

    let addEdges (edges: Edge list) (creasePattern: CreasePattern) : CreasePattern =
        let vertices: Point2D list =
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
            with
            | _ -> creasePattern


    /// Try adding an edge to the crease pattern. If the edge already exists, the same crease pattern will be returned
    let addEdge (edge: Edge) (creasePattern: CreasePattern) : CreasePattern = addEdges [ edge ] creasePattern

    (* Builder *)

    ///  Create a basic crease pattern within a 0 -> 1 grid system with edge boundaries
    let create: CreasePattern =
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

    let performAxiom axiom creasePattern =
        let line = Axiom.perform axiom

        creasePattern



    (* Queries *)

    // This function is currently linear but can be sped up with quad-trees
    /// Get the closest vertex that is withing a particular distance
    let pointWithin distance vertex (CreasePattern creasePattern) : Point2D option =
        let distanceSquared = distance * distance
        let distSquaredToVertex = Point2D.distanceSquaredTo vertex

        let vertexDistance, closestVertex =
            Vertices.tovertexList creasePattern.graph
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

    /// Get the closest edge that is withing a particular distance
    let edgeWithin (distance: float) (vertex: Point2D) (creasePattern: CreasePattern) : Edge option =
        let defaultCase =
            (infinity,
             Edge.betweenWithAssignment
                 (Point2D.xy infinity infinity)
                 (Point2D.xy -infinity -infinity)
                 EdgeAssignment.Unassigned)

        let closestDistance, closestEdge =
            edges creasePattern
            |> List.fold
                (fun closestEdge nextEdge ->
                    match closestEdge with
                    | closestDistance, _ as closestEdge ->
                        let nextDistance =
                            nextEdge.crease
                            |> LineSegment2D.distanceToPoint vertex

                        if nextDistance < closestDistance then
                            (nextDistance, nextEdge)
                        else
                            closestEdge)
                defaultCase

        if closestDistance < distance then
            Some closestEdge
        else
            None


    (* Serialization & Deserialization *)

    /// Convert the frame values in the fold file to a crease pattern
    let fromFoldFrame (frame: Fold.Frame) : CreasePattern =
        let coordinates = Array.ofList frame.vertices.coords

        let mapEdgeAssignment assignment =
            match assignment with
            | Fold.Boundary -> Boundary
            | Fold.Mountain -> Mountain
            | Fold.Valley -> Valley
            | Fold.Unassigned -> Unassigned
            | Fold.Flat -> Unassigned

        let mapUnit unit =
            match unit with
            | Fold.Meters -> Meters
            | Fold.Points -> Pixels
            | Fold.Unitless -> Unitless
            | _ -> Unitless

        let graphEdges =
            List.map2
                (fun (first, second) -> Edge.betweenWithAssignment coordinates.[first] coordinates.[second])
                frame.edges.vertices
                (List.map mapEdgeAssignment frame.edges.assignment)

        empty
        |> addEdges graphEdges
        |> setUnit (mapUnit frame.unit)
        |> setAuthor frame.author
        |> setTitle frame.title
        |> setDescription frame.description

    /// Add in the crease pattern values into the fold frame
    let toFoldFrame (creasePattern: CreasePattern) : Fold.Frame =
        let vertexLookup: Map<Point2D, int> =
            vertices creasePattern
            |> List.indexed
            |> List.fold (fun map (id, vert) -> Map.add vert id map) Map.empty

        let vertices =
            Fold.Vertices.create
                { coords = vertices creasePattern
                  vertices = []
                  faces = [] }

        let edgeVertices: (Point2D * Point2D) list =
            List.map Edge.vertices (edges creasePattern)

        let edgeVerticesIndexed =
            List.map (fun (v1, v2) -> (Map.find v1 vertexLookup, Map.find v2 vertexLookup)) edgeVertices

        let mapEdgeAssignment assignment =
            match assignment with
            | Boundary -> Fold.Boundary
            | Mountain -> Fold.Mountain
            | Valley -> Fold.Valley
            | Flat -> Fold.Flat
            | Unassigned -> Fold.Unassigned

        let foldEdges =
            Fold.Edges.create
                { vertices = edgeVerticesIndexed
                  faces = []
                  assignment = List.map (Edge.assignment >> mapEdgeAssignment) (edges creasePattern)
                  foldAngle = []
                  length = []
                  orders = [] }

        let mapUnit unit =
            match unit with
            | Meters -> Fold.Meters
            | Pixels -> Fold.Points
            | Unitless -> Fold.Unitless


        Fold.Frame.empty
        |> Fold.Frame.setUnit (mapUnit creasePattern.unit)
        |> Fold.Frame.setAuthor creasePattern.author
        |> Fold.Frame.setTitle creasePattern.title
        |> Fold.Frame.setDescription creasePattern.description
        |> Fold.Frame.setVertices vertices
        |> Fold.Frame.setEdges foldEdges
