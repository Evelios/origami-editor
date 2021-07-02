module GeometryTests.Line2D

open FsCheck
open NUnit.Framework
open Geometry

[<SetUp>]
let Setup () = ()


let pointOnLineTestCases =
    let line =
        Line2D.unsafeFromTo (Point2D.xy 0. 5.) (Point2D.xy 5. 5.)

    [ (Point2D.xy 0. 5.), line
      (Point2D.xy 5. 5.), line
      (Point2D.xy 2.5 5.), line
      (Point2D.xy 2.5 (5. + Generics.Epsilon / 2.), line) ]
    |> List.map TestCaseData

[<TestCaseSource(nameof pointOnLineTestCases)>]
let ``Vertex is on line`` vertex line =
    Assert.That(Line2D.isPointOnLine vertex line)

// Need to create a generator for Line2D objects that creates objects with different starting points
//[<Test>]
//let ``Line perpendicular through point`` () =
//    let perpTests (point: Point2D) (line: Line2D) =
//        let perpLine = Line2D.perpThroughPoint point line
//
//        Line2D.isPerpendicularTo perpLine line
//        && Line2D.pointOnLine point perpLine
//
//    Assert.DoesNotThrow(fun () -> Check.QuickThrowOnFailure perpTests)
