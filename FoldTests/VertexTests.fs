module FoldTests.VertexTests

open NUnit.Framework
open Geometry

[<SetUp>]
let Setup () = ()

let equality2dCases =
    [ TestCaseData(Vertex.in2d 0. 0., Vertex.in2d 0. 0.)
      TestCaseData(Vertex.in2d 1. 1., Vertex.in2d 1. 1.)
      TestCaseData(Vertex.in2d 1000. 1000., Vertex.in2d 1000. 1000.)
      TestCaseData(Vertex.in2d 0.1 0.1, Vertex.in2d 0.1 0.1)
      TestCaseData(Vertex.in2d 0.000000001 0.000000001, Vertex.in2d 0.000000002 0.000000002) ]

[<TestCaseSource("equality2dCases")>]
let equality2d first second = Assert.IsTrue(first.Equals(second))
