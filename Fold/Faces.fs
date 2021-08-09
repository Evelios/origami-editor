namespace Fold

type Faces =
    { vertices: int list list
      edges: int list list
      orders: (int (*face id*)  * int (*face id*)  * int (*order*) ) list }

module Faces =

    let create a: Faces = a

    let empty: Faces =
        { vertices = []
          edges = []
          orders = [] }
