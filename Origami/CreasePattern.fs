namespace CreasePattern

open Fold.Json
open Geometry

type Label = int

type CreasePattern =
    | CreasePattern of CreasePatternData

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

and CreasePatternData =
    { bounds: BoundingBox2D
      graph: Graph
      unit: LengthUnit
      author: string
      title: string
      description: string }


module CreasePattern =
    open FSharp.FGL.Directed
    open Utilities.Extensions

    (* Create a crease pattern using the "Unitless" units and a square sheet of paper. The crease pattern is created in
     * the square sheet of paper using cartesian coordinates. The bottom left corner is the origin (0, 0) and the paper
     * has a width and height of 1.
     *)
    let empty : CreasePattern =
        CreasePattern
            { bounds = BoundingBox2D.empty
              graph = Graph.empty
              unit = Unitless
              author = ""
              title = ""
              description = "" }

    (* Accessors *)

    let private asEdge (start, finish, assignment) =
        Edge.betweenWithAssignment start finish assignment

    let size (CreasePattern creasePattern) =
        Size.create
            (creasePattern.bounds.maxX
             - creasePattern.bounds.minX)
            (creasePattern.bounds.maxY
             - creasePattern.bounds.minY)

    let edges (CreasePattern creasePattern) : Edge seq = Graph.edges creasePattern.graph

    let vertices (CreasePattern creasePattern) : Point2D seq = Graph.vertices creasePattern.graph

    let elements creasePattern : GraphElement seq =
        Seq.append (Seq.map VertexElement (vertices creasePattern)) (Seq.map EdgeElement (edges creasePattern))



    (* Modifiers *)

    let private setBoundingBox boundingBox (CreasePattern creasePattern) =
        CreasePattern
            { creasePattern with
                  bounds = boundingBox }

    let private expandBoundingBox vertices (CreasePattern cpData as creasePattern) =
        setBoundingBox (BoundingBox2D.containingPoints vertices cpData.bounds) creasePattern

    let setUnit unit (CreasePattern creasePattern) =
        CreasePattern { creasePattern with unit = unit }

    let setAuthor author (CreasePattern creasePattern) =
        CreasePattern { creasePattern with author = author }

    let setTitle title (CreasePattern creasePattern) =
        CreasePattern { creasePattern with title = title }

    let setDescription description (CreasePattern creasePattern) =
        CreasePattern
            { creasePattern with
                  description = description }

    let private mapGraph f (CreasePattern creasePatternData) =
        CreasePattern
            { creasePatternData with
                  graph = f creasePatternData.graph }

    let addVertex vertex creasePattern : CreasePattern =
        creasePattern
        |> expandBoundingBox [ vertex ]
        |> mapGraph (Graph.addVertex vertex)

    /// Add vertices, if any of the vertices lie outside the bounding box of the current crease pattern, then then
    /// bounds are expanded to encompass the bounding box.
    let addVertices vertices creasePattern : CreasePattern =
        creasePattern
        |> expandBoundingBox vertices
        |> mapGraph (Graph.addVertices vertices)

    /// Add multiple edge creases to the crease pattern. If the edge does not lie within the current bounds of the
    /// crease pattern, this function expands the bounding box.
    let addEdges (edges: Edge seq) creasePattern : CreasePattern =
        creasePattern
        |> expandBoundingBox (Edge.seqVertices edges)
        |> mapGraph (Graph.addEdges edges)


    /// Try adding an edge to the crease pattern. If the edge already exists, the same crease pattern will be returned
    let addEdge (edge: Edge) creasePattern : CreasePattern =
        creasePattern
        |> expandBoundingBox (Tuple2.toList (Edge.vertices edge))
        |> mapGraph (Graph.addEdges [ edge ])

    /// Run the axiom and add in the line segment(s) created by the axiom onto the crease pattern
    let performAxiom axiom (CreasePattern cpData as creasePattern) =
        Axiom.perform axiom
        |> List.filterMap (Boolean2D.boundingBoxAndLine cpData.bounds)
        |> List.map (fun line -> Edge.atWithAssignment line EdgeAssignment.Unassigned)
        |> fun edges -> addEdges edges creasePattern

    (* Queries *)

    /// Get the vertex in the crease pattern that is the closest to a given vertex
    let closestVertex vertex (CreasePattern creasePatternData) : (Point2D * float) option =
        let distSquaredToVertex = Point2D.distanceSquaredTo vertex

        let closestDistanceSquared, closestVertex =
            Graph.vertices creasePatternData.graph
            |> Seq.fold
                (fun (distanceSquared, closestVertex) nextVertex ->
                    let nextDistanceSquared = distSquaredToVertex nextVertex

                    if nextDistanceSquared < distanceSquared then
                        (nextDistanceSquared, nextVertex)

                    else
                        (distanceSquared, closestVertex))
                (infinity, Point2D.xy infinity infinity)

        if closestDistanceSquared < infinity then
            Some(closestVertex, sqrt closestDistanceSquared)
        else
            None

    // This function is currently linear but can be sped up with quad-trees
    /// Get the closest vertex that is withing a particular distance
    let pointWithin maxDist vertex creasePattern : Point2D option =
        match closestVertex vertex creasePattern with
        | Some (vertex, distance) ->
            if distance < maxDist then
                Some vertex
            else
                None
        | None -> None

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
            |> Seq.fold
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

    (* Builders *)

    /// Create a rectangular crease pattern from it's bounding box
    let withBoundingBox boundingBox =
        let corners = BoundingBox2D.corners boundingBox

        let boundaries =
            [ Edge.betweenWithAssignment corners.tl corners.tr EdgeAssignment.Boundary
              Edge.betweenWithAssignment corners.tr corners.br EdgeAssignment.Boundary
              Edge.betweenWithAssignment corners.br corners.bl EdgeAssignment.Boundary
              Edge.betweenWithAssignment corners.bl corners.tl EdgeAssignment.Boundary ]

        empty
        |> setBoundingBox boundingBox
        |> addEdges boundaries

    ///  Create a basic crease pattern within a 0 -> 1 grid system with edge boundaries
    let create : CreasePattern =
        BoundingBox2D.from (Point2D.xy 1. 1.) (Point2D.xy 0. 0.)
        |> withBoundingBox


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
            | Fold.Flat -> Flat

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
        let vertexLookup : Map<Point2D, int> =
            vertices creasePattern
            |> Seq.indexed
            |> Seq.fold (fun map (id, vert) -> Map.add vert id map) Map.empty

        let vertices =
            Fold.Vertices.create
                { coords = vertices creasePattern |> List.ofSeq
                  vertices = []
                  faces = [] }

        let edgeVertices : (Point2D * Point2D) list =
            Seq.map Edge.vertices (edges creasePattern)
            |> List.ofSeq

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
                  assignment =
                      Seq.map (Edge.assignment >> mapEdgeAssignment) (edges creasePattern)
                      |> List.ofSeq
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


    // Todo: Remove this declaration. This is temporarily done to avoid naming clashes
    open Fold

    let toJson creasePattern =
        Fold.empty
        |> Fold.setKeyFrame (toFoldFrame creasePattern)
        |> FoldJson.toJson

    let fromJson json =
        FoldJson.fromJson json
        |> (fun fold -> fold.keyFrame)
        |> fromFoldFrame
