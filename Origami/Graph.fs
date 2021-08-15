namespace CreasePattern

open Geometry
open Utilities.Collections
open Utilities.Extensions

type PointId = int

type Graph = Graph of GraphData

and GraphData =
    { vertices: Map<PointId, Point2D>
      edges: Map<UnorderedTuple2<PointId>, EdgeAssignment> }

module Graph =

    (* Builder *)
    let empty =
        Graph
            { vertices = Map.empty
              edges = Map.empty }

    (* Accessors *)

    /// This function is unsafe and assumes that the graph is well maintained
    ///
    /// <exception cref="KeyNotFoundException">
    ///     Raised when the point is not in the graph. This shouldn't happen
    ///     unless the graph was put into an invalid state.
    /// </exception>
    let private getVertex pointId (Graph graphData) = Map.find pointId graphData.vertices

    // TODO: this check may be problematic. Ensure that the id isn't sensitive to tolerances
    /// Determine if the point is in the graph. This point checks for both equality and id existence
    let private vertexExists (vertex: Point2D) (Graph graphData) =
        Map.exists (fun id point -> vertex.GetHashCode() = id && vertex = point) graphData.vertices

    /// Check to see if the edge exists in the graph. The edge is checked for assignment and endpoint location. The
    /// endpoints can ve in any order since the graph is undirected.
    let private edgeExists (edge: Edge) (Graph graphData) =
        let endpointIds =
            UnorderedTuple2.from (edge.crease.start.GetHashCode()) (edge.crease.finish.GetHashCode())

        Map.exists
            (fun existingIds assignment ->
                edge.assignment = assignment
                && endpointIds = existingIds)
            graphData.edges

    /// Get all the edges within the graph
    let edges (Graph graphData as graph) =
        graphData.edges
        |> Map.toSeq
        |> Seq.map
            (fun (endpointIds, assignment) ->
                Edge.betweenWithAssignment
                    (getVertex (UnorderedTuple2.fst endpointIds) graph)
                    (getVertex (UnorderedTuple2.snd endpointIds) graph)
                    assignment)

    /// Get all the vertices within the graph
    let vertices (Graph graphData) = Map.values graphData.vertices


    (* Modifiers *)

    /// Adds any new vertices to the graph. Any vertices that already exist will not be added
    let addVertices vertices (Graph graphData as graph) =
        let verticesToAdd =
            vertices
            |> Seq.filter (fun vertex -> not <| vertexExists vertex graph)

        let newVerticesMap =
            Seq.fold
                (fun accGraph vertex -> Map.add (vertex.GetHashCode()) vertex accGraph)
                graphData.vertices
                verticesToAdd

        Graph
            { graphData with
                  vertices = newVerticesMap }

    /// Adds any vertex to the graph. Any vertices that already exist will not be added
    let addVertex vertex graph = addVertices [ vertex ] graph

    let private addEdgesUnsafe (edges: Edge seq) (Graph graphData) =
        let newEdgeMap =
            Seq.fold
                (fun accGraph (edge: Edge) ->
                    Map.add
                        (UnorderedTuple2.ofTuple (edge.crease.start.GetHashCode(), edge.crease.finish.GetHashCode()))
                        edge.assignment
                        accGraph)
                graphData.edges
                edges

        Graph { graphData with edges = newEdgeMap }


    /// Add edges to the graph
    let addEdges (edges: Edge seq) graph =
        let newVertices =
            Seq.map
                (fun (edge: Edge) ->
                    [ edge.crease.start
                      edge.crease.finish ])
                edges
            |> Seq.concat

        let newEdges =
            Seq.filter (fun edge -> not <| edgeExists edge graph) edges

        graph
        |> addVertices newVertices
        |> addEdgesUnsafe newEdges


    /// Add a single edge to the graph
    let addEdge (edge: Edge) graph = addEdges [ edge ] graph

    let removeEdge edge graph = failwith "" // TODO: implement me
    let removeEdges edges graph = failwith "" // TODO: implement me
