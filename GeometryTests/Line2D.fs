module GeometryTests.Line2D

open FsCheck
open NUnit.Framework
open Geometry

[<SetUp>]
let Setup () = ()

let pointClosestToTestCases =
    let line =
        Line2D.through (Point2D.xy 0. 5.) (Point2D.xy 5. 5.)

    [ (Point2D.xy 0. 5.), line, (Point2D.xy 0. 5.)
      (Point2D.xy 5. 5.), line, (Point2D.xy 5. 5.)
      (Point2D.xy 2. 2.), line, (Point2D.xy 2. 5.) ]
    |> List.map (fun (point, line, expected) -> TestCaseData(point, line).Returns(expected))

[<TestCaseSource(nameof pointClosestToTestCases)>]
let ``Point closest to line`` vertex line = Line2D.pointClosestTo vertex line

let pointOnLineTestCases =
    let line =
        Line2D.through (Point2D.xy 0. 5.) (Point2D.xy 5. 5.)

    [ (Point2D.xy 0. 5.), line
      (Point2D.xy 5. 5.), line
      (Point2D.xy 2.5 5.), line
      (Point2D.xy 2.5 (5. + Internal.Epsilon / 2.), line) ]
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

[<Test>]
let ``Line Intersection`` () =
    let l1 =
        Line2D.through (Point2D.xy 1. 4.) (Point2D.xy 4. 1.)

    let l2 =
        Line2D.through (Point2D.xy 1. 1.) (Point2D.xy 4. 4.)

    let expected = Some(Point2D.xy 2.5 2.5)
    let actual = Line2D.intersect l1 l2
    Assert.AreEqual(expected, actual)
