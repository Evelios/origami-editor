module Utilities.HashCode

[<Literal>]
let private Prime1= 2654435761u
[<Literal>]
let private Prime2= 2246822519u
[<Literal>]
let private Prime3= 3266489917u
[<Literal>]
let private Prime4= 668265263u
[<Literal>]
let private Prime5= 374761393u

let seed = System.Random().Next()

let combine seq =
    Seq.fold (fun hash x ->  x.GetHashCode() ) seed seq