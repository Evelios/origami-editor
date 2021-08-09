module Utilities.Debug

let log name x =
    printfn $"{name}: {x}"
    x