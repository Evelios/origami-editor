module CreasePatternCanvasTests

open Avalonia
open Gui.Components.CreasePatternCanvas
open NUnit.Framework
open CreasePattern
open Geometry
open Gui

[<SetUp>]
let Setup () = ()

type TestCase =
    { messages: CreasePatternCanvas.Msg list
      expected: State }

let testCases =
    [ { messages = []; expected = Shell.init }
      { messages = [ CreasePatternCanvas.Msg.MouseMove(Point(500., 500.)) ]
        expected =
            { Shell.init with
                  mousePosition = Some(Point(500., 500.))
                  vertexPosition = Some(Point2D.xy 1. 1.)
                  hover = Point2D.xy 1. 1. |> VertexComponent |> Some } } ]
    |> List.map
        (fun testCase ->
            TestCaseData(testCase.messages)
                .Returns(testCase.expected))

[<TestCaseSource(nameof testCases)>]
let ``Update Tests`` messages =
    List.fold (fun state msg -> CreasePatternCanvas.update msg state) Shell.init messages
