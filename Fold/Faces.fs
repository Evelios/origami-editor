namespace Fold

type Faces =
    { Vertices: int list list
      Edges: int list list
      Orders: (int (*face id*)  * int (*face id*)  * int (*order*) ) list }

module Faces =
    let create a : Faces = a

    let empty : Faces =
        { Vertices = []
          Edges = []
          Orders = [] }
