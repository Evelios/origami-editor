namespace Utilities.Extensions

module List =
    
    /// Append the first list if the condition is met
    let concatIf condition first second : 'a list =
        if condition then
            second @ first
        else
            second
            
    let appendWhenSome maybeElement list: 'a list =
        match maybeElement with
            | Some element -> list @ [element]
            | None -> list