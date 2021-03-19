module FoldTests.Util

open NUnit.Framework

let toTest (testCases: ('a*'b) list): TestCaseData list =
    List.map (fun (d, r) -> TestCaseData(d :> obj).Returns r) testCases

let toTestReverse (testCases: ('a*'b) list): TestCaseData list =
    List.map (fun (r, d) -> TestCaseData(d :> obj).Returns r) testCases
