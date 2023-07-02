module CreasePatternTests.Graph

open Math.Geometry
open Math.Units
open NUnit.Framework
open FsCheck.NUnit

open Origami
open Math.Geometry.Test
open Utilities
open Utilities.Test

[<SetUp>]
let SetUp () = Gen.ArbGeometry.Register()


[<Property>]
let ``Vertex insert order independent`` (vertices: Point2D<Meters, OrigamiCoordinates> list) =
    Graph.addVertices vertices Graph.empty
    .=. Graph.addVertices (List.rev vertices) Graph.empty

[<Property>]
let ``Edge insert order independent`` (creases: LineSegment2D<Meters, OrigamiCoordinates> list) =
    let edges =
        List.map (fun crease -> Edge.atWithAssignment crease EdgeAssignment.Flat) creases

    Graph.addEdges edges Graph.empty
    .=. Graph.addEdges (List.rev edges) Graph.empty

[<Property>]
let ``Edge start point and end point order independent`` (edges: Edge list) =
    let reversedEdges =
        List.map
            (fun (edge: Edge) -> Edge.betweenWithAssignment edge.Crease.Finish edge.Crease.Start edge.Assignment)
            edges

    Graph.addEdges edges Graph.empty
    .=. Graph.addEdges reversedEdges Graph.empty
