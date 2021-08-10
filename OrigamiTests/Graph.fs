module CreasePatternTests.Graph

open NUnit.Framework

open GeometryTests
open Utilities

[<SetUp>]
let SetUp () = Gen.ArbGeometry.Register()
