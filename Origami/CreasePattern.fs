namespace CreasePattern

open Geometry

type Label = int

type CreasePattern =
    { Bounds: BoundingBox2D
      Graph: Graph
      Unit: LengthUnit
      Author: string
      Title: string
      Description: string }


module CreasePattern =
    open FSharp.FGL.Directed
    open Utilities.Extensions

    (* Create a crease pattern using the "Unitless" units and a square sheet of paper. The crease pattern is created in
     * the square sheet of paper using cartesian coordinates. The bottom left corner is the origin (0, 0) and the paper
     * has a width and height of 1.
     *)
    let empty : CreasePattern =
        { Bounds = BoundingBox2D.empty
          Graph = Graph.empty
          Unit = Unitless
          Author = ""
          Title = ""
          Description = "" }

    (* Accessors *)

    let private asEdge (start, finish, assignment) =
        Edge.betweenWithAssignment start finish assignment

    /// Get the size (width and height) of the crease pattern. This size is invariant of any translations on the
    /// crease pattern.
    let size creasePattern =
        Size.create
            (creasePattern.Bounds.MaxX
             - creasePattern.Bounds.MinX)
            (creasePattern.Bounds.MaxY
             - creasePattern.Bounds.MinY)

    /// Get all the edges that are in the crease pattern. This includes all the boundary edges from the paper.
    let edges creasePattern : Edge seq = Graph.edges creasePattern.Graph

    /// Get all the vertices that are a part of the crease pattern
    let vertices creasePattern : Point2D seq = Graph.vertices creasePattern.Graph

    /// Get all the vertices and edges that are a part of the crease pattern
    let elements creasePattern : GraphElement seq =
        Seq.append (Seq.map VertexElement (vertices creasePattern)) (Seq.map EdgeElement (edges creasePattern))



    (* Modifiers *)

    /// Set the bounding box of the current crease pattern.
    let private setBoundingBox boundingBox creasePattern =
        { creasePattern with
              Bounds = boundingBox }

    /// Expand the bounding box of the crease pattern to
    let private expandBoundingBox vertices creasePattern =
        setBoundingBox (BoundingBox2D.containingPoints vertices creasePattern.Bounds) creasePattern

    /// Set the length unit that is being used within the crease pattern.
    /// TODO: This does not perform any unit conversions within the crease pattern.
    let setUnit unit creasePattern = { creasePattern with Unit = unit }

    /// Set the author of the crease pattern
    let setAuthor author creasePattern = { creasePattern with Author = author }

    /// Set the title of the crease pattern
    let setTitle title creasePattern = { creasePattern with Title = title }

    /// Set the description of the crease pattern
    let setDescription description creasePattern =

        { creasePattern with
              Description = description }

    /// Perform an action on the underlying vertex and edge graph object
    let private mapGraph f creasePattern =
        { creasePattern with
              Graph = f creasePattern.Graph }

    /// Add a vertex to the crease pattern.If any of the vertices lie outside the bounding box of the current crease
    /// pattern, then then bounds are expanded to encompass the bounding box.
    let addVertex vertex creasePattern : CreasePattern =
        creasePattern
        |> expandBoundingBox [ vertex ]
        |> mapGraph (Graph.addVertex vertex)

    /// Add vertices to the crease pattern. if any of the vertices lie outside the bounding box of the current crease
    /// pattern, then then bounds are expanded to encompass the bounding box.
    let addVertices vertices creasePattern : CreasePattern =
        creasePattern
        |> expandBoundingBox vertices
        |> mapGraph (Graph.addVertices vertices)

    /// Add multiple edge creases to the crease pattern. If the edge does not lie within the current bounds of the
    /// crease pattern, this function expands the bounding box. If edges run through existing edges, the intersection
    /// of those edges are also added to the crease pattern.
    let addEdges (edges: Edge seq) creasePattern : CreasePattern =
        creasePattern
        |> expandBoundingBox (Edge.seqVertices edges)
        |> mapGraph (Graph.addEdges edges)


    /// Try adding an edge to the crease pattern. If the edge already exists, the same crease pattern will be returned.
    let addEdge (edge: Edge) creasePattern : CreasePattern =
        creasePattern
        |> expandBoundingBox (Tuple2.toList (Edge.vertices edge))
        |> mapGraph (Graph.addEdges [ edge ])

    /// Create a crease along a line. This line will be bounded within the workable region of the crease pattern. This
    /// function helps convert infinite lines into ones that are limited by the workable region of the paper.
    let boundedEdge line creasePattern =
        let maybeSegment =
            Boolean2D.boundingBoxAndLine creasePattern.Bounds line

        Option.map (fun lineSegment -> Edge.atWithAssignment lineSegment EdgeAssignment.Unassigned) maybeSegment

    /// Get all the edges created within the crease pattern by performing a particular axiom action.
    let axiomResult assignment axiom creasePattern =
        Axiom.perform axiom
        |> List.ofSeq
        |> List.filterMap (Boolean2D.boundingBoxAndLine creasePattern.Bounds)
        |> List.map (fun line -> Edge.atWithAssignment line assignment)

    /// Get all the edges created within the crease pattern by performing multiple axiom actions on it.
    let axiomPreviews assignment axiom creasePattern =
        let edges =
            axiomResult assignment axiom creasePattern

        let cpVertices = Set.ofSeq (vertices creasePattern)

        let vertices =
            Edge.seqVertices edges
            |> Set.ofSeq
            |> Set.difference cpVertices

        Seq.append (Seq.map EdgeElement edges) (Seq.map VertexElement vertices)

    /// Run the axiom and add in the line segment(s) created by the axiom onto the crease pattern
    let performAxiom assignment axiom creasePattern =
        axiomResult assignment axiom creasePattern
        |> fun edges -> addEdges edges creasePattern

    /// Perform a sequence of axioms on the crease pattern
    let performAxioms assignment axioms creasePattern =
        Seq.fold (fun cp axiom -> performAxiom assignment axiom cp) creasePattern axioms

    (* Queries *)

    /// Get the vertex in the crease pattern that is the closest to a given vertex
    let closestVertex vertex creasePattern : (Point2D * float) option =
        let distSquaredToVertex = Point2D.distanceSquaredTo vertex

        let closestDistanceSquared, closestVertex =
            Graph.vertices creasePattern.Graph
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

    /// Get the closest edge that is withing a particular distance from the input point.
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
                            nextEdge.Crease
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

    /// Create a rectangular crease pattern from it's bounding box.
    let withBoundingBox boundingBox =
        let boundaries =
            BoundingBox2D.lineSegments boundingBox
            |> List.map (fun crease -> Edge.atWithAssignment crease EdgeAssignment.Boundary)

        empty
        |> setBoundingBox boundingBox
        |> addEdges boundaries

    ///  Create a basic crease pattern within a 0 -> 1 grid system with edge boundaries.
    let create : CreasePattern =
        BoundingBox2D.from (Point2D.xy 1. 1.) (Point2D.xy 0. 0.)
        |> withBoundingBox


    (* Serialization & Deserialization *)

    /// Convert the frame values in the fold file to a crease pattern
    let fromFoldFrame (frame: Fold.Frame) : CreasePattern =
        let coordinates = Array.ofList frame.Vertices.Coordinates

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
                frame.Edges.Vertices
                (List.map mapEdgeAssignment frame.Edges.Assignment)

        let creasePattern =
            empty
            |> addEdges graphEdges
            |> setUnit (mapUnit frame.Unit)
            |> setAuthor frame.Author
            |> setTitle frame.Title
            |> setDescription frame.Description

        creasePattern

    /// Add in the crease pattern values into the fold frame
    let toFoldFrame (creasePattern: CreasePattern) : Fold.Frame =
        let vertexLookup : Map<Point2D, int> =
            vertices creasePattern
            |> Seq.indexed
            |> Seq.fold (fun map (id, vert) -> Map.add vert id map) Map.empty

        let vertices =
            Fold.Vertices.create
                { Coordinates = vertices creasePattern |> List.ofSeq
                  Vertices = []
                  Faces = [] }

        let edgeVertices : (Point2D * Point2D) list =
            edges creasePattern
            |> Seq.map Edge.vertices
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
            | Preview -> Fold.Unassigned

        let foldEdges =
            Fold.Edges.create
                { Vertices = edgeVerticesIndexed
                  Faces = []
                  Assignment =
                      Seq.map (Edge.assignment >> mapEdgeAssignment) (edges creasePattern)
                      |> List.ofSeq
                  FoldAngle = []
                  Length = []
                  Orders = [] }

        let mapUnit unit =
            match unit with
            | Meters -> Fold.Meters
            | Pixels -> Fold.Points
            | Unitless -> Fold.Unitless


        Fold.Frame.empty
        |> Fold.Frame.setUnit (mapUnit creasePattern.Unit)
        |> Fold.Frame.setAuthor creasePattern.Author
        |> Fold.Frame.setTitle creasePattern.Title
        |> Fold.Frame.setDescription creasePattern.Description
        |> Fold.Frame.setVertices vertices
        |> Fold.Frame.setEdges foldEdges



    let toJson creasePattern =
        Fold.Fold.empty
        |> Fold.Fold.setKeyFrame (toFoldFrame creasePattern)
        |> Fold.Fold.toJson

    let fromJson json =
        Fold.Fold.fromJson json
        |> (fun fold -> fold.KeyFrame)
        |> fromFoldFrame
