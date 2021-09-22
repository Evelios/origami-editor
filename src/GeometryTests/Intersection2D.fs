module GeometryTests.Intersection

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Geometry
open Utilities

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

[<Test>]
let ``Line Segment And Line Intersection`` () =
    let segment =
        LineSegment2D.from (Point2D.xy 1. 4.) (Point2D.xy 4. 1.)

    let line =
        Line2D.through (Point2D.xy 1. 1.) (Point2D.xy 4. 4.)

    let expected = Some(Point2D.xy 2.5 2.5)

    let actual =
        Intersection2D.lineSegmentAndLine segment line

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Line Segment And Line No Intersection`` () =
    let segment =
        LineSegment2D.from (Point2D.xy 1. 4.) (Point2D.xy 2. 3.)

    let line =
        Line2D.through (Point2D.xy 1. 1.) (Point2D.xy 4. 4.)

    let expected = None

    let actual =
        Intersection2D.lineSegmentAndLine segment line

    Assert.AreEqual(expected, actual)


let circleIntersectionTestCases =
    [ "Disjoint Circles", Circle2D.from Point2D.origin 5., Circle2D.from (Point2D.xy 10. 0.) 3., Seq.empty
      "Circle Containing Circle", Circle2D.from Point2D.origin 5., Circle2D.from (Point2D.xy 1. 0.) 3., Seq.empty
      "Tangent circles",
      Circle2D.from Point2D.origin 5.,
      Circle2D.from (Point2D.xy 10. 0.) 5.,
      Seq.singleton (Point2D.xy 5. 0.)
      "Intersecting circles",
      Circle2D.from Point2D.origin 10.,
      Circle2D.from (Point2D.xy 10. 0.) 10.,
      Seq.ofArray [| Point2D.xy 5. (5. * sqrt 3.)
                     Point2D.xy 5. -(5. * sqrt 3.) |] ]
    |> List.map
        (fun (name, c1, c2, expected) ->
            TestCaseData(c1, c2)
                .SetName(name)
                .Returns(expected))

[<TestCaseSource(nameof circleIntersectionTestCases)>]
let ``Circle Intersections`` c1 c2 = Intersection2D.circles c1 c2

[<Property>]
let ``Circle Has Tangent Intersections`` c1 c2 =
    Intersection2D.circles c1 c2
    |> Seq.forall
        (fun t ->
            Circle2D.isTangentPoint t c1
            && Circle2D.isTangentPoint t c2)
