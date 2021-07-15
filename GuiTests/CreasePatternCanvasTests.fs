module CreasePatternCanvasTests

open Avalonia
open Geometry
open Gui.Components.CreasePatternCanvas
open NUnit.Framework
open CreasePattern
open Geometry
open Gui

[<SetUp>]
let Setup () = ()

type TestCase =
    { name: string
      messages: CreasePatternCanvas.Msg list
      expected: State }

let testCases =
    [ { name = "Do nothing"
        messages = []
        expected = Shell.init }

      { name = "Vertex Hover"
        messages = [ CreasePatternCanvas.Msg.MouseMove(Point(500., 500.)) ]
        expected =
            { Shell.init with
                  mousePosition = Some(Point(500., 500.))
                  vertexPosition = Some(Point2D.xy 1. 1.)
                  hover = Point2D.xy 1. 1. |> VertexComponent |> Some } }

      { name = "Crease edge by dragging between points"
        messages =
            [ CreasePatternCanvas.Msg.MousePressed(Point(500., 500.))
              CreasePatternCanvas.Msg.MouseReleased(Point(0., 0.)) ]
        expected =
            { Shell.init with
                  mousePosition = Some(Point(0., 0.))
                  vertexPosition = Some(Point2D.xy 0. 0.)
                  hover = Point2D.xy 0. 0. |> VertexComponent |> Some
                  creasePattern =
                      CreasePattern.create
                      |> CreasePattern.addEdge (
                          Edge.betweenWithAssignment (Point2D.xy 1. 1.) (Point2D.xy 0. 0.) Unassigned
                      ) } }

      { name = "Press near point & release off point"
        messages =
            [ CreasePatternCanvas.Msg.MousePressed(Point(500., 500.))
              CreasePatternCanvas.Msg.MouseReleased(Point(200., 200.)) ]
        expected =
            { Shell.init with
                  mousePosition = Some(Point(200., 200.))
                  vertexPosition = Some(Point2D.xy 0.4 0.4) } } ]
    |> List.map
        (fun testCase ->
            TestCaseData(testCase.messages)
                .Returns(testCase.expected)
                .SetName(testCase.name))

[<TestCaseSource(nameof testCases)>]
let ``Update Tests`` messages =
    List.fold (fun state msg -> CreasePatternCanvas.update msg state) Shell.init messages
