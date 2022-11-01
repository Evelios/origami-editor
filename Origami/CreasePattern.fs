[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Origami.CreasePattern

open Math.Geometry
open Math.Units
open FSharp.FGL.Directed

open Utilities.Extensions
open Origami.Fold

/// Create a crease pattern using the "Unitless" units and a square sheet of paper. The crease pattern is created in
/// the square sheet of paper using cartesian coordinates. The bottom left corner is the origin (0, 0) and the paper
/// has a width and height of 1.
///
let empty<'Coordinates> : CreasePattern<'Coordinates> =
    { Bounds = BoundingBox2D.empty
      Graph = Graph.empty
      Author = ""
      Title = ""
      Description = "" }

// ---- Accessors --------------------------------------------------------------

let private asEdge (start, finish, assignment) =
    Edge.betweenWithAssignment start finish assignment

/// Get the size (width and height) of the crease pattern. This size is invariant of any translations on the
/// crease pattern.
let size creasePattern =
    Size2D.create
        (creasePattern.Bounds.MaxX
         - creasePattern.Bounds.MinX)
        (creasePattern.Bounds.MaxY
         - creasePattern.Bounds.MinY)

/// Get all the edges that are in the crease pattern. This includes all the boundary edges from the paper.
let edges (creasePattern: CreasePattern<'Coordinates>) : Edge<'Coordinates> seq = Graph.edges creasePattern.Graph

/// Get all the vertices that are a part of the crease pattern
let vertices (creasePattern: CreasePattern<'Coordinates>) : Point2D<Meters, 'Coordinates> seq =
    Graph.vertices creasePattern.Graph

/// Get all the vertices and edges that are a part of the crease pattern
let elements (creasePattern: CreasePattern<'Coordinates>) : GraphElement<'Coordinates> seq =
    Seq.append (Seq.map VertexElement (vertices creasePattern)) (Seq.map EdgeElement (edges creasePattern))


// ---- Modifiers --------------------------------------------------------------

/// Set the bounding box of the current crease pattern.
let private setBoundingBox boundingBox creasePattern =
    { creasePattern with Bounds = boundingBox }

/// Expand the bounding box of the crease pattern to
let private expandBoundingBox
    (vertices: Point2D<Meters, 'Coordinates> seq)
    (creasePattern: CreasePattern<'Coordinates>)
    : CreasePattern<'Coordinates> =
    setBoundingBox (BoundingBox2D.containingPoints (List.ofSeq vertices) creasePattern.Bounds) creasePattern

/// Set the author of the crease pattern
let setAuthor author (creasePattern: CreasePattern<'Coordinates>) = { creasePattern with Author = author }

/// Set the title of the crease pattern
let setTitle title (creasePattern: CreasePattern<'Coordinates>) = { creasePattern with Title = title }

/// Set the description of the crease pattern
let setDescription description (creasePattern: CreasePattern<'Coordinates>) =

    { creasePattern with Description = description }

/// Perform an action on the underlying vertex and edge graph object
let private mapGraph f (creasePattern: CreasePattern<'Coordinates>) =
    { creasePattern with Graph = f creasePattern.Graph }

/// Add a vertex to the crease pattern.If any of the vertices lie outside the bounding box of the current crease
/// pattern, then then bounds are expanded to encompass the bounding box.
let addVertex vertex (creasePattern: CreasePattern<'Coordinates>) : CreasePattern<'Coordinates> =
    creasePattern
    |> expandBoundingBox [ vertex ]
    |> mapGraph (Graph.addVertex vertex)

/// Add vertices to the crease pattern. if any of the vertices lie outside the bounding box of the current crease
/// pattern, then then bounds are expanded to encompass the bounding box.
let addVertices vertices (creasePattern: CreasePattern<'Coordinates>) : CreasePattern<'Coordinates> =
    creasePattern
    |> expandBoundingBox vertices
    |> mapGraph (Graph.addVertices vertices)

/// Add multiple edge creases to the crease pattern. If the edge does not lie within the current bounds of the
/// crease pattern, this function expands the bounding box. If edges run through existing edges, the intersection
/// of those edges are also added to the crease pattern.
let addEdges
    (edges: Edge<'Coordinates> seq)
    (creasePattern: CreasePattern<'Coordinates>)
    : CreasePattern<'Coordinates> =
    creasePattern
    |> expandBoundingBox (Edge.seqVertices edges)
    |> mapGraph (Graph.addEdges edges)


/// Try adding an edge to the crease pattern. If the edge already exists, the same crease pattern will be returned.
let addEdge (edge: Edge<'Coordinates>) (creasePattern: CreasePattern<'Coordinates>) : CreasePattern<'Coordinates> =
    creasePattern
    |> expandBoundingBox (Tuple2.toList (Edge.vertices edge))
    |> mapGraph (Graph.addEdges [ edge ])

/// Create a crease along a line. This line will be bounded within the workable region of the crease pattern. This
/// function helps convert infinite lines into ones that are limited by the workable region of the paper.
let boundedEdge line (creasePattern: CreasePattern<'Coordinates>) : Edge<'Coordinates> option =
    let maybeSegment =
        Boolean2D.boundingBoxAndLine creasePattern.Bounds line

    Option.map (fun lineSegment -> Edge.atWithAssignment lineSegment EdgeAssignment.Unassigned) maybeSegment

/// Get all the edges created within the crease pattern by performing a particular axiom action.
let axiomResult assignment axiom (creasePattern: CreasePattern<'Coordinates>) : Edge<'Coordinates> seq =
    Axiom.perform axiom
    |> List.ofSeq
    |> List.filterMap (Boolean2D.boundingBoxAndLine creasePattern.Bounds)
    |> List.map (fun line -> Edge.atWithAssignment line assignment)
    |> List.toSeq

/// Get all the edges created within the crease pattern by performing multiple axiom actions on it.
let axiomPreviews assignment axiom (creasePattern: CreasePattern<'Coordinates>) : GraphElement<'Coordinates> seq =
    let edges =
        axiomResult assignment axiom creasePattern

    let cpVertices =
        Set.ofSeq (vertices creasePattern)

    let vertices =
        Edge.seqVertices edges
        |> Set.ofSeq
        |> Set.difference cpVertices

    Seq.append (Seq.map EdgeElement edges) (Seq.map VertexElement vertices)

/// Run the axiom and add in the line segment(s) created by the axiom onto the crease pattern
let performAxiom assignment axiom (creasePattern: CreasePattern<'Coordinates>) : CreasePattern<'Coordinates> =
    axiomResult assignment axiom creasePattern
    |> fun edges -> addEdges edges creasePattern

/// Perform a sequence of axioms on the crease pattern
let performAxioms assignment axioms (creasePattern: CreasePattern<'Coordinates>) : CreasePattern<'Coordinates> =
    Seq.fold (fun cp axiom -> performAxiom assignment axiom cp) creasePattern axioms


// ---- Queries ----------------------------------------------------------------

/// Get the vertex in the crease pattern that is the closest to a given vertex
let closestVertex
    vertex
    (creasePattern: CreasePattern<'Coordinates>)
    : (Point2D<Meters, 'Coordinates> * Length) option =
    let distSquaredToVertex =
        Point2D.distanceSquaredTo vertex

    let (closestDistanceSquared: Quantity<SquareMeters>), closestVertex =
        Graph.vertices creasePattern.Graph
        |> Seq.fold
            (fun (distanceSquared, closestVertex) nextVertex ->
                let nextDistanceSquared =
                    distSquaredToVertex nextVertex

                if nextDistanceSquared < distanceSquared then
                    (nextDistanceSquared, nextVertex)

                else
                    (distanceSquared, closestVertex))
            (Quantity.infinity, Point2D.meters infinity infinity)

    if closestDistanceSquared < Quantity.infinity then
        Some(closestVertex, Quantity.sqrt closestDistanceSquared)
    else
        None

// This function is currently linear but can be sped up with quad-trees
/// Get the closest vertex that is withing a particular distance
let pointWithin maxDist vertex (creasePattern: CreasePattern<'Coordinates>) : Point2D<Meters, 'Coordinates> option =
    match closestVertex vertex creasePattern with
    | Some (vertex, distance) ->
        if distance < maxDist then
            Some vertex
        else
            None
    | None -> None

/// Get the closest edge that is withing a particular distance from the input point.
let edgeWithin
    (distance: Length)
    (vertex: Point2D<Meters, 'Coordinates>)
    (creasePattern: CreasePattern<'Coordinates>)
    : Edge<'Coordinates> option =
    let defaultCase: Length * Edge<'Coordinates> =
        (Quantity.infinity,
         Edge.betweenWithAssignment
             (Point2D.meters infinity infinity)
             (Point2D.meters -infinity -infinity)
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


// ---- Builders ---------------------------------------------------------------

/// Create a rectangular crease pattern from it's bounding box.
let withBoundingBox boundingBox =
    let boundaries =
        BoundingBox2D.lineSegments boundingBox
        |> List.map (fun crease -> Edge.atWithAssignment crease EdgeAssignment.Boundary)

    empty
    |> setBoundingBox boundingBox
    |> addEdges boundaries

///  Create a basic crease pattern within a 0 -> 1 grid system with edge boundaries.
let create<'Coordinates> : CreasePattern<'Coordinates> =
    BoundingBox2D.from
        (Point2D.xy (Quantity.create 1.) (Quantity.create 1.))
        (Point2D.xy (Quantity.create 0.) (Quantity.create 0.))
    |> withBoundingBox


// ---- Serialization & Deserialization ----------------------------------------

/// Convert the frame values in the fold file to a crease pattern
let fromFoldFrame (frame: Frame<'Coordiantes>) : CreasePattern<'Coordiantes> =
    let coordinates =
        Array.ofList frame.Vertices.Coordinates

    let graphEdges =
        List.map2
            (fun (first, second) -> Edge.betweenWithAssignment coordinates[first] coordinates[second])
            frame.Edges.Vertices
            frame.Edges.Assignment

    let creasePattern =
        empty
        |> addEdges graphEdges
        |> setAuthor frame.Author
        |> setTitle frame.Title
        |> setDescription frame.Description

    creasePattern

/// Add in the crease pattern values into the fold frame
let toFoldFrame (creasePattern: CreasePattern<'Coordinates>) : Fold.Frame<'Coordinates> =
    let vertexLookup: Map<Point2D<Meters, 'Coordinates>, int> =
        vertices creasePattern
        |> Seq.indexed
        |> Seq.fold (fun map (id, vert) -> Map.add vert id map) Map.empty

    let vertices =
        Vertices.create
            { Coordinates = vertices creasePattern |> List.ofSeq
              Vertices = []
              Faces = [] }

    let edgeVertices: (Point2D<Meters, 'Coordinates> * Point2D<Meters, 'Coordinates>) list =
        edges creasePattern
        |> Seq.map Edge.vertices
        |> List.ofSeq

    let edgeVerticesIndexed =
        List.map (fun (v1, v2) -> (Map.find v1 vertexLookup, Map.find v2 vertexLookup)) edgeVertices

    let foldEdges =
        Edges.create
            { Vertices = edgeVerticesIndexed
              Faces = []
              Assignment =
                Seq.map Edge.assignment (edges creasePattern)
                |> List.ofSeq
              FoldAngle = []
              Length = []
              Orders = [] }


    Frame.empty
    |> Frame.setAuthor creasePattern.Author
    |> Frame.setTitle creasePattern.Title
    |> Frame.setDescription creasePattern.Description
    |> Frame.setVertices vertices
    |> Frame.setEdges foldEdges


let toJson creasePattern =
    Fold.empty
    |> Fold.setKeyFrame (toFoldFrame creasePattern)
    |> Fold.toJson

let fromJson json =
    Fold.fromJson json
    |> Result.map ((fun fold -> fold.KeyFrame) >> fromFoldFrame)
