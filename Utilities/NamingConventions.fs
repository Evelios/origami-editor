namespace Utilities

open System.Text.RegularExpressions

module NamingConventions =
    /// Converts camel case input into space separated camel case input
    let upperCaseSpaceSeparated (input: string): string =
        Regex.Replace(input, "(\\B[A-Z])", " $1")
