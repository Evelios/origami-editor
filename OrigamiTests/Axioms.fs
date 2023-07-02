module CreasePatternTests.Axioms

open NUnit.Framework
open FsCheck
open FsCheck.NUnit

open Origami
open Math.Geometry
open Math.Geometry.Test

[<SetUp>]
let SetUp () = Gen.ArbGeometry.Register()

[<Property>]
let ``Axiom one: points are on resulting line`` p1 p2 =
    let result = Axiom.first p1 p2

    Line2D.isPointOnLine p1 result
    && Line2D.isPointOnLine p2 result
