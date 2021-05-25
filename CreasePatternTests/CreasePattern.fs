module CreasePatternTests.CreasePattern

open NUnit.Framework
open CreasePattern
open Fold

[<SetUp>]
let Setup () = ()

[<Test>]
let addingRedundantVertices () =
    let given =
        CreasePattern.create
        |> CreasePattern.addVertices [ Vertex.in2d 0. 0.
                                       Vertex.in2d 1. 1. ]

    let actual =
        given
        |> CreasePattern.addVertices [ Vertex.in2d 0. 0.
                                       Vertex.in2d 1. 1. ]

    Assert.AreEqual(given, actual)
