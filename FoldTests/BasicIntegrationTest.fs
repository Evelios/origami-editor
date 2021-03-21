module FoldTests.BasicIntegrationTest

open System.IO
open NUnit.Framework
open Fold

[<SetUp>]
let Setup () = ()

[<Test>]
let BasicIntegration () =
    let actual =
        File.FromJson
            (File.ReadAllText
             <| Path.Join(__SOURCE_DIRECTORY__, "basic-integration.json"))

    let expected =
        File.Create
            { spec = Some(Version.Create 0 0 1)
              creator = Some "Thomas Waters"
              author = Some "Thomas Waters"
              title = Some "Fold Spec v0.0.1"
              description = Some "Fold Integration Test"
              classes = Some [ FileClass.Diagrams ]
              keyFrame =
                  Some
                      (Frame.Create
                          { author = Some "Thomas Waters"
                            title = Some "Key Frame"
                            description = Some "The Mainframe"
                            classes =
                                Some [ FrameClass.Graph
                                       FrameClass.Linkage ]
                            attributes =
                                Some [ FrameAttribute.Geo2D
                                       FrameAttribute.Manifold
                                       FrameAttribute.NonSelfIntersecting ]
                            unit = Some Unit.Unitless
                            vertices =
                                Some
                                    (Vertices.Create
                                        { vertices = Some [ 0; 1; 2; 3 ]
                                          faces = Some [ [ 0; 1 ]; [ 1; 2 ]; [ 2; 3 ] ]
                                          coords =
                                              Some [ (0., 0.)
                                                     (0., 1.)
                                                     (1., 0.)
                                                     (1., 1.) ] })
                            edges =
                                Some
                                    (Edges.Create
                                        { vertices = Some [ (0, 1); (1, 2); (2, 3) ]
                                          faces = Some [ (0, 1, 2, 3); (2, 3, 4, 5) ]
                                          assignment =
                                              Some [ EdgeAssignment.Boundary
                                                     EdgeAssignment.Boundary
                                                     EdgeAssignment.Mountain
                                                     EdgeAssignment.Valley ]
                                          foldAngle = Some [90.;90.;90.]
                                          length = Some [1.; 1.; 1.]
                                          orders = None })
                            faces =
                                Some
                                    (Faces.Create
                                        { vertices = Some [[0;1;2];[1;2;3;]]
                                          edges = Some [[0;1;2];[1;2;3]]
                                          orders = None }) })
              frames = None }

    Assert.AreEqual(expected, actual)
