namespace Origami

open Math.Geometry
open Math.Units

open Utilities.Collections
open Utilities.Extensions

type PointId = int

type Graph =
    { Vertices: Map<PointId, Point2D<Meters, OrigamiCoordinates>>
      Edges: Map<UnorderedTuple2<PointId>, EdgeAssignment> }

module Graph =

    (* Builder *)
    let empty =
        { Vertices = Map.empty
          Edges = Map.empty }

    (* Accessors *)

    /// This function is unsafe and assumes that the graph is well maintained
    ///
    /// <exception cref="KeyNotFoundException">
    ///     Raised when the point is not in the graph. This shouldn't happen
    ///     unless the graph was put into an invalid state.
    /// </exception>
    let private getVertex pointId graph = Map.find pointId graph.Vertices

    // TODO: this check may be problematic. Ensure that the id isn't sensitive to tolerances
    /// Determine if the point is in the graph. This point checks for both equality and id existence
    let private vertexExists (vertex: Point2D<Meters, OrigamiCoordinates>) graph =
        Map.exists (fun id point -> vertex.GetHashCode() = id && vertex = point) graph.Vertices

    /// Check to see if the edge exists in the graph. The edge is checked for assignment and endpoint location. The
    /// endpoints can ve in any order since the graph is undirected.
    let private edgeExists (edge: Edge) graph =
        let endpointIds =
            UnorderedTuple2.from (edge.Crease.Start.GetHashCode()) (edge.Crease.Finish.GetHashCode())

        Map.exists
            (fun existingIds assignment ->
                edge.Assignment = assignment
                && endpointIds = existingIds)
            graph.Edges

    /// Get all the edges within the graph
    let edges graph =
        graph.Edges
        |> Map.toSeq
        |> Seq.map
            (fun (endpointIds, assignment) ->
                Edge.betweenWithAssignment
                    (getVertex (UnorderedTuple2.fst endpointIds) graph)
                    (getVertex (UnorderedTuple2.snd endpointIds) graph)
                    assignment)
        |> Seq.sort

    /// Get all the vertices within the graph
    let vertices graph = Map.values graph.Vertices |> Seq.sort


    (* Modifiers *)

    /// Adds any new vertices to the graph. Any vertices that already exist will not be added
    let addVertices vertices graph =
        let verticesToAdd =
            vertices
            |> Seq.filter (fun vertex -> not <| vertexExists vertex graph)

        let newVerticesMap =
            Seq.fold
                (fun accGraph vertex -> Map.add (vertex.GetHashCode()) vertex accGraph)
                graph.Vertices
                verticesToAdd

        { graph with Vertices = newVerticesMap }

    /// Adds any vertex to the graph. Any vertices that already exist will not be added
    let addVertex vertex graph = addVertices [ vertex ] graph

    let private addEdgesUnsafe (edges: Edge seq) graph =
        let newEdgeMap =
            Seq.fold
                (fun accGraph (edge: Edge) ->
                    Map.add
                        (UnorderedTuple2.ofTuple (edge.Crease.Start.GetHashCode(), edge.Crease.Finish.GetHashCode()))
                        edge.Assignment
                        accGraph)
                graph.Edges
                edges

        { graph with Edges = newEdgeMap }


    /// Add edges to the graph
    let addEdges (newEdges: Edge seq) graph =
        let newEdges =
            newEdges
            |> Seq.filter (fun edge -> not <| edgeExists edge graph)
            |> Seq.map Edge.round

        let intersections =
            Seq.cartesian (edges graph) newEdges
            |> List.ofSeq
            |> List.filterMap (fun (e1, e2) -> LineSegment2D.intersectionPoint e1.Crease e2.Crease)

        let newVertices =
            Edge.seqVertices newEdges
            |> Seq.append intersections
            |> Seq.map Point2D.round
            |> Seq.distinct

        graph
        |> addVertices newVertices
        |> addEdgesUnsafe newEdges


    /// Add a single edge to the graph
    let addEdge (edge: Edge) graph = addEdges [ edge ] graph
