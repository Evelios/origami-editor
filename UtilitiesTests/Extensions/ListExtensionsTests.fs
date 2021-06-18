module UtilitiesTests.Extensions.ListExtensionsTests

open NUnit.Framework
open Utilities.Extensions

[<SetUp>]
let Setup () = ()

[<Test>]
let ``Append True`` =
    // Given
    let first = [ 1; 2; 3 ]
    let second = [ 4; 5; 6 ]

    let expected = first
    let actual = List.concatIf true first second
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Append False`` =
    // Given
    let first = [ 1; 2; 3 ]
    let second = [ 4; 5; 6 ]

    let expected = [ 1; 2; 3; 4; 5; 6 ]
    let actual = List.concatIf true first second
    Assert.AreEqual(expected, actual)
