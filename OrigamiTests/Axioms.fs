module CreasePatternTests.Axioms

open CreasePattern
open Geometry
open FsCheck
open NUnit.Framework

// Todo: Create generators for points
//[<Test>]
//let ``Axiom one: points are on resulting line`` () =
//    let axiomOne (p1: Point2D) (p2: Point2D) =
//        let result = Axioms.first p1 p2
//
//        Line2D.isPointOnLine p1 result
//        && Line2D.isPointOnLine p2 result
//
//    Check.QuickThrowOnFailure axiomOne
