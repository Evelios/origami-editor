module GeometryTests.Point2D

open NUnit.Framework
open Geometry

[<SetUp>]
let Setup () = ()


let vertexEqualityTestCases =
    [ (Point2D.xy 0. 0., Point2D.xy 0. 0.)
      (Point2D.xy 1. 1., Point2D.xy 1. 1.)
      (Point2D.xy 1. 1., Point2D.xy (1. + Generics.Epsilon / 2.) (1. + Generics.Epsilon / 2.)) ]
    |> List.map TestCaseData

[<TestCaseSource(nameof vertexEqualityTestCases)>]
let ``Vertices are equal`` (lhs: Point2D) (rhs: Point2D) =
    Assert.AreEqual(lhs, rhs)
    Assert.That(lhs.Equals(rhs))


let vertexLessThanTestCases =
    [ (Point2D.xy 0. 0., Point2D.xy 1. 1.)
      (Point2D.xy 0. 0., Point2D.xy 0. 1.) ]
    |> List.map TestCaseData

[<TestCaseSource(nameof vertexLessThanTestCases)>]
let ``Vertex less than`` (lhs: Point2D) (rhs: Point2D) = Assert.Less(lhs, rhs)
