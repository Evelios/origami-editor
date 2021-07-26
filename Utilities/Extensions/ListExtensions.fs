namespace Utilities.Extensions

module List =

    /// Append the first list if the condition is met
    let concatIf condition first second : 'a list =
        if condition then
            second @ first
        else
            second

    /// Append the first list if the condition is met
    let appendIf condition first second : 'a list =
        if condition then
            first @ second
        else
            second

    let appendWhenSome maybeElement list : 'a list =
        match maybeElement with
        | Some element -> list @ [ element ]
        | None -> list

    let consWhenSome maybeElement list : 'a list =
        match maybeElement with
        | Some e -> e :: list
        | None -> list

    /// Get all unique pairs of a list
    let rec pairs l =
        match l with
        | h :: t ->
            [ for x in t do
                yield h, x
              yield! pairs t ]
        | _ -> []
