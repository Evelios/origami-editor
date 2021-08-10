module Utilities.Debug

let log name x =
    printfn $"{name}: {x}"
    x

let print name x = printfn $"{name}: {x}"
