namespace Fold

type Faces =
    { vertices: int list list
      edges: int list list
      orders: (int (*face id*)  * int (*face id*)  * int (*order*) ) list }

module Faces =

    let Create a: Faces = a

    let Empty: Faces =
        { vertices = []
          edges = []
          orders = [] }
