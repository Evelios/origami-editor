module FoldTests.BasicIntegrationTest

open System.IO
open NUnit.Framework
open Fold

[<SetUp>]
let Setup () = ()

[<Test>]
let BasicIntegration () =
    let actual =
        FoldFile.FromJson
            (File.ReadAllText
             <| Path.Combine(__SOURCE_DIRECTORY__, "basic-integration.fold"))

    let expected =
        FoldFile.Create
            { spec = 1
              creator = "Thomas Waters"
              author = "Thomas Waters"
              title = "Fold Spec v0.0.1"
              description = "Fold Integration Test"
              classes = Set.ofList [ FileClass.Diagrams ]
              keyFrame =
                  (Frame.Create
                      { author = "Thomas Waters"
                        title = "Key Frame"
                        description = "The Mainframe"
                        classes =
                            Set.ofList [ FrameClass.Graph
                                         FrameClass.Linkage ]
                        attributes =
                            Set.ofList [ FrameAttribute.Geo2D
                                         FrameAttribute.Manifold
                                         FrameAttribute.NonSelfIntersecting ]
                        unit = Unit.Unitless
                        vertices =

                            (Vertices.Create
                                { vertices = Some [ 0; 1; 2; 3 ]
                                  faces = Some [ [ 0; 1 ]; [ 1; 2 ]; [ 2; 3 ] ]
                                  coords =
                                      Some [ Vertex.in2d 0.f 0.f
                                             Vertex.in2d 0.f 1.f
                                             Vertex.in2d 1.f 0.f
                                             Vertex.in2d 1.f 1.f ] })
                        edges =

                            (Edges.Create
                                { vertices = Some [ (0, 1); (1, 2); (2, 3) ]
                                  faces = Some [ (1, Some 2); (2, None) ]
                                  assignment =
                                      Some [ EdgeAssignment.Boundary
                                             EdgeAssignment.Boundary
                                             EdgeAssignment.Mountain
                                             EdgeAssignment.Valley ]
                                  foldAngle = Some [ 90.; 90.; 90. ]
                                  length = Some [ 1.; 1.; 1. ]
                                  orders = None })
                        faces =

                            (Faces.Create
                                { vertices = Some [ [ 0; 1; 2 ]; [ 1; 2; 3 ] ]
                                  edges = Some [ [ 0; 1; 2 ]; [ 1; 2; 3 ] ]
                                  orders = None }) })
              frames = [] }

    Assert.AreEqual(expected, actual)
