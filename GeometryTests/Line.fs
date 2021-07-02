module GeometryTests.LineSegment

open NUnit.Framework
open Geometry

[<SetUp>]
let Setup () = ()


let pointOnLineTestCases =
    let line =
        Line2D.fromTo (Point2D.xy 0. 5.) (Point2D.xy 5. 5.)

    [ (Point2D.xy 0. 5.), line
      (Point2D.xy 5. 5.), line
      (Point2D.xy 2.5 5.), line
      (Point2D.xy 2.5 (5. + Generics.Epsilon / 2.), line) ]
    |> List.map TestCaseData

[<TestCaseSource(nameof pointOnLineTestCases)>]
let ``Vertex is on line`` vertex line =
    Assert.That(Line2D.pointOnLine vertex line)
