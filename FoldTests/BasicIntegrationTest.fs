module FoldTests.BasicIntegrationTest

open System.IO
open NUnit.Framework

open Fold
open Geometry

[<SetUp>]
let Setup () = ()

let json =
    Path.Combine(__SOURCE_DIRECTORY__, "basic-integration.fold")
    |> File.ReadAllText

let foldFile =
    Fold.create
        { Spec = 1
          Creator = "Thomas Waters"
          Author = "Thomas Waters"
          Title = "Fold Spec v0.0.1"
          Description = "Fold Integration Test"
          Classes = Set.ofList [ FileClass.Diagrams ]
          KeyFrame =
              (Frame.create
                  { Author = "Thomas Waters"
                    Title = "Key Frame"
                    Description = "The Mainframe"
                    Classes =
                        Set.ofList [ FrameClass.Graph
                                     FrameClass.Linkage ]
                    Attributes =
                        Set.ofList [ FrameAttribute.Geo2D
                                     FrameAttribute.Manifold
                                     FrameAttribute.NonSelfIntersecting ]
                    Unit = LengthUnit.Unitless
                    Vertices =

                        (Vertices.create
                            { Vertices = [ 0; 1; 2; 3 ]
                              Faces = [ [ 0; 1 ]; [ 1; 2 ]; [ 2; 3 ] ]
                              Coordinates =
                                  [ Point2D.xy 0. 0.
                                    Point2D.xy 0. 1.
                                    Point2D.xy 1. 0.
                                    Point2D.xy 1. 1. ] })
                    Edges =

                        (Edges.create
                            { Vertices = [ (0, 1); (1, 2); (2, 3) ]
                              Faces = [ (1, Some 2); (2, None) ]
                              Assignment =
                                  [ EdgeAssignment.Boundary
                                    EdgeAssignment.Boundary
                                    EdgeAssignment.Mountain
                                    EdgeAssignment.Valley ]
                              FoldAngle = [ 90.; 90.; 90. ]
                              Length = [ 1.; 1.; 1. ]
                              Orders = [ (2, 1, -1) ] })
                    Faces =

                        (Faces.create
                            { Vertices = [ [ 0; 1; 2 ]; [ 1; 2; 3 ] ]
                              Edges = [ [ 0; 1; 2 ]; [ 1; 2; 3 ] ]
                              Orders = [ (1, 2, 0) ] }) })
          Frames = [] }

[<Test>]
let Deserialization () =
    let actual = Fold.fromJson json
    Assert.AreEqual(foldFile, actual)

[<Test>]
let Serialization () =
    let actual = Fold.toJson foldFile
    Assert.AreEqual(json, actual)
