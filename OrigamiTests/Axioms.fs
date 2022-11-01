module CreasePatternTests.Axioms

open NUnit.Framework
open FsCheck
open FsCheck.NUnit

open Origami
open Math.Units
open Math.Geometry


[<SetUp>]
let SetUp () = Gen.ArbOrigami.Register()

[<Property>]
let ``Axiom one: points are on resulting line`` p1 p2 =
    let result = Axiom.first p1 p2

    Line2D.isPointOnLine p1 result
    && Line2D.isPointOnLine p2 result
