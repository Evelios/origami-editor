module CreasePatternTests.Axioms

open CreasePattern
open Geometry
open FsCheck
open NUnit.Framework

[<Test>]
let ``Reverse of reverse of a list is the original list`` () =
    let revRevIsOrig (xs: list<int>) = List.rev (List.rev xs) = xs
    Check.QuickThrowOnFailure revRevIsOrig

[<Test>]
let ``Axiom one: points are on resulting line`` () =
    let axiomOne (p1: Point2D) (p2: Point2D) =
        Axioms.first p1 p2
        |> Option.map
            (fun line ->
                Line2D.isPointOnLine p1 line
                && Line2D.isPointOnLine p2 line)
        |> Option.defaultValue true

    Check.QuickThrowOnFailure axiomOne
