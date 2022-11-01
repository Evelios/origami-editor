[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Origami.Graph

open Math.Geometry
open Math.Units
open Utilities.Collections
open Utilities.Extensions


// ---- Builders ----------------------------------------------------------------

let empty =
    { Vertices = Map.empty
      Edges = Map.empty }

// ---- Accessors ---------------------------------------------------------------

/// <summary>
///   This function is unsafe and assumes that the graph is well maintained
/// </summary>
///
/// <exception cref="KeyNotFoundException">
///   Raised when the point is not in the graph. This shouldn't happen
///   unless the graph was put into an invalid state.
/// </exception>
let private getVertex (pointId: PointId) (graph: Graph<'Coordinates>) : Point2D<Meters, 'Coordinates> =
    Map.find pointId graph.Vertices

// TODO: this check may be problematic. Ensure that the id isn't sensitive to tolerances
/// Determine if the point is in the graph. This point checks for both equality and id existence
let private vertexExists (vertex: Point2D<Meters, 'Coordinates>) graph =
    Map.exists (fun id point -> vertex.GetHashCode() = id && vertex = point) graph.Vertices

/// Check to see if the edge exists in the graph. The edge is checked for assignment and endpoint location. The
/// endpoints can ve in any order since the graph is undirected.
let private edgeExists (edge: Edge<'Coordinates>) (graph: Graph<'Coordinates>) : bool =
    let endpointIds =
        UnorderedTuple2.from (edge.Crease.Start.GetHashCode()) (edge.Crease.Finish.GetHashCode())

    Map.exists
        (fun existingIds assignment ->
            edge.Assignment = assignment
            && endpointIds = existingIds)
        graph.Edges

/// Get all the edges within the graph
let edges (graph: Graph<'Coordinates>) : Edge<'Coordinates> seq =
    graph.Edges
    |> Map.toSeq
    |> Seq.map (fun (endpointIds, assignment) ->
        Edge.betweenWithAssignment
            (getVertex (UnorderedTuple2.fst endpointIds) graph)
            (getVertex (UnorderedTuple2.snd endpointIds) graph)
            assignment)
    |> Seq.sort

/// Get all the vertices within the graph
let vertices graph = Map.values graph.Vertices |> Seq.sort


// ---- Modifiers ---------------------------------------------------------------

/// Adds any new vertices to the graph. Any vertices that already exist will not be added
let addVertices (vertices: Point2D<Meters, 'Coordinates> seq) (graph: Graph<'Coordinates>) : Graph<'Coordinates> =
    let verticesToAdd =
        vertices
        |> Seq.filter (fun vertex -> not <| vertexExists vertex graph)

    let newVerticesMap =
        Seq.fold (fun accGraph vertex -> Map.add (vertex.GetHashCode()) vertex accGraph) graph.Vertices verticesToAdd

    { graph with Vertices = newVerticesMap }

/// Adds any vertex to the graph. Any vertices that already exist will not be added
let addVertex vertex graph = addVertices [ vertex ] graph

let private addEdgesUnsafe (edges: Edge<'Coordinates> seq) graph =
    let newEdgeMap =
        Seq.fold
            (fun accGraph (edge: Edge<'Coordinates>) ->
                Map.add
                    (UnorderedTuple2.ofTuple (hash edge.Crease.Start, hash edge.Crease.Finish))
                    edge.Assignment
                    accGraph)
            graph.Edges
            edges

    { graph with Edges = newEdgeMap }


// TODO: Fix Me
module LineSegment2D =
    let intersect (a: LineSegment2D<Meters, 'Coordinates>) (_: LineSegment2D<Meters, 'Coordinates>) = Some a.Start


/// Add edges to the graph
let addEdges (newEdges: Edge<'Coordinates> seq) (graph: Graph<'Coordinates>) : Graph<'Coordinates> =
    let newEdges =
        newEdges
        |> Seq.filter (fun edge -> not <| edgeExists edge graph)
        |> Seq.map Edge.round

    let intersections =
        Seq.cartesian (edges graph) newEdges
        |> List.ofSeq
        |> List.filterMap (fun (e1, e2) -> LineSegment2D.intersect e1.Crease e2.Crease)

    let newVertices =
        Edge.seqVertices newEdges
        |> Seq.append intersections
        |> Seq.map Point2D.round
        |> Seq.distinct

    graph
    |> addVertices newVertices
    |> addEdgesUnsafe newEdges


/// Add a single edge to the graph
let addEdge (edge: Edge<'Coordinates>) (graph: Graph<'Coordinates>) : Graph<'Coordinates> = addEdges [ edge ] graph
