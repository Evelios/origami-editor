module CreasePatternTests.Graph

open NUnit.Framework
open FsCheck.NUnit

open CreasePattern
open GeometryTests
open Utilities
open Utilities.Test

[<SetUp>]
let SetUp () = Gen.ArbGeometry.Register()


[<Property>]
let ``Vertex insert order independent`` vertices =
    Graph.addVertices vertices Graph.empty
    .=. Graph.addVertices (List.rev vertices) Graph.empty

[<Property>]
let ``Edge insert order independent`` creases =
    let edges =
        List.map (fun crease -> Edge.atWithAssignment crease EdgeAssignment.Flat) creases

    Graph.addEdges edges Graph.empty
    .=. Graph.addEdges (List.rev edges) Graph.empty

[<Property>]
let ``Edge start point and end point order independent`` edges =
    let reversedEdges =
        List.map
            (fun (edge: Edge) -> Edge.betweenWithAssignment edge.Crease.Finish edge.Crease.Start edge.Assignment)
            edges

    Graph.addEdges edges Graph.empty
    .=. Graph.addEdges reversedEdges Graph.empty
