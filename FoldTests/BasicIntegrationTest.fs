module FoldTests.BasicIntegrationTest

open System.IO
open NUnit.Framework
open Fold

[<SetUp>]
let Setup () = ()

let json =
    Path.Combine(__SOURCE_DIRECTORY__, "basic-integration.fold")
    |> File.ReadAllText

let foldFile =
    Fold.Create
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
                            { vertices = [ 0; 1; 2; 3 ]
                              faces = [ [ 0; 1 ]; [ 1; 2 ]; [ 2; 3 ] ]
                              coords =
                                  [ Vertex.in2d 0.f 0.f
                                    Vertex.in2d 0.f 1.f
                                    Vertex.in2d 1.f 0.f
                                    Vertex.in2d 1.f 1.f ] })
                    edges =

                        (Edges.Create
                            { vertices = [ (0, 1); (1, 2); (2, 3) ]
                              faces = [ (1, Some 2); (2, None) ]
                              assignment =
                                  [ EdgeAssignment.Boundary
                                    EdgeAssignment.Boundary
                                    EdgeAssignment.Mountain
                                    EdgeAssignment.Valley ]
                              foldAngle = [ 90.; 90.; 90. ]
                              length = [ 1.; 1.; 1. ]
                              orders = [ (2, 1, -1) ] })
                    faces =

                        (Faces.Create
                            { vertices = [ [ 0; 1; 2 ]; [ 1; 2; 3 ] ]
                              edges = [ [ 0; 1; 2 ]; [ 1; 2; 3 ] ]
                              orders = [ (1, 2, 0) ] }) })
          frames = [] }

[<Test>]
let Deserialization () =
    let actual = FoldJson.FromJson json
    Assert.AreEqual(foldFile, actual)

[<Test>]
let Serialization () =
    let actual = FoldJson.ToJson foldFile
    Assert.AreEqual(json, actual)
