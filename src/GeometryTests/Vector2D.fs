module GeometryTests.Vector2D

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Geometry
open Utilities

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

[<Test>]
let ``Vector from polar`` () =
    let expected = Vector2D.xy 0. 1.
    let actual = Vector2D.ofPolar 1. (Angle.pi / 2.)

    Assert.AreEqual(expected, actual)

[<Property>]
let ``Equality and hash code comparison with random points`` (first: Vector2D) (second: Vector2D) =
    (first = second) = (first.GetHashCode() = second.GetHashCode())
