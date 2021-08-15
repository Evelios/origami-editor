module CreasePatternTests.CreasePattern

open NUnit.Framework
open CreasePattern
open Geometry
open Utilities.Extensions

[<SetUp>]
let Setup () = ()

let redundantElementsTestCases =
    let v1 = Point2D.xy 0. 0.
    let v2 = Point2D.xy 1. 1.

    let edge =
        Edge.betweenWithAssignment v1 v2 EdgeAssignment.Unassigned

    [ ("Adding Redundant Vertices",
       CreasePattern.create
       |> CreasePattern.addVertices [ v1; v2 ],
       CreasePattern.create
       |> CreasePattern.addVertices [ v1; v2 ]
       |> CreasePattern.addVertices [ v1; v2 ])

      ("Adding Redundant Edges",
       CreasePattern.create |> CreasePattern.addEdge edge,
       CreasePattern.create
       |> CreasePattern.addEdge edge
       |> CreasePattern.addEdge edge) ]

    |> List.map (fun (name, given, actual) -> TestCaseData(given, actual).SetName(name))

[<TestCaseSource(nameof redundantElementsTestCases)>]
let ``Adding Redundant Elements`` given actual = Assert.AreEqual(given, actual)

[<Test>]
let ``Get edges from crease pattern`` () =
    let edges =
        [ Point2D.xy 0. 0., Point2D.xy 1. 1., EdgeAssignment.Flat
          Point2D.xy 1. 1., Point2D.xy 0. 0., EdgeAssignment.Flat
          Point2D.xy 1. 0., Point2D.xy 0. 1., EdgeAssignment.Flat ]
        |> List.map (Tuple3.map Edge.betweenWithAssignment)

    let creasePatternEdges =
        CreasePattern.empty
        |> CreasePattern.addEdges edges
        |> CreasePattern.edges

    printf $"{creasePatternEdges}"

    CollectionAssert.AreEqual(edges, creasePatternEdges)

[<Literal>]
let closeDistance = 0.1

let pointCloseTestCases =
    let pointAdd = Point2D.xy 0.5 0.5

    [ ("Point Not Close", pointAdd, Point2D.xy 0.1 0.1, None)
      ("Point Close", pointAdd, Point2D.xy 0.5 0.45, Some pointAdd)
      ("Point On Boundary", pointAdd, Point2D.xy 0.5 0.4, Some pointAdd) ]

    |> List.map
        (fun (name, add, test, result) ->
            TestCaseData(add, test)
                .Returns(result)
                .SetName(name))

[<TestCaseSource(nameof pointCloseTestCases)>]
let ``Point Close To Vertex`` add test =
    CreasePattern.create
    |> CreasePattern.addVertices [ add ]
    |> CreasePattern.pointWithin closeDistance test


let edgeCloseTestCases =
    let edgeAdd =
        Edge.betweenWithAssignment (Point2D.xy 0. 0.5) (Point2D.xy 0.5 0.5) EdgeAssignment.Unassigned

    [ ("Point Not Close", edgeAdd, Point2D.xy 0.1 0.1, None)
      ("Point on line", edgeAdd, Point2D.xy 0.3 0.5, Some edgeAdd)
      ("Point near perpendicular", edgeAdd, Point2D.xy 0.3 0.5, Some edgeAdd)
      ("Point near endpoint", edgeAdd, Point2D.xy 0.55 0.5, Some edgeAdd) ]
    |> List.map
        (fun (name, add, test, result) ->
            TestCaseData(add, test)
                .Returns(result)
                .SetName(name))

[<TestCaseSource(nameof edgeCloseTestCases)>]
let ``Point Close To Edge`` edge test =
    CreasePattern.create
    |> CreasePattern.addEdge edge
    |> CreasePattern.edgeWithin closeDistance test
