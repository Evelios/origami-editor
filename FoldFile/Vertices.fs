namespace Fold

type Vertices =
    { coords: Vertex list
      vertices: int list
      faces: int list list }

module Vertices =

    let create a: Vertices = a

    let empty: Vertices =
        { coords = []
          vertices = []
          faces = [] }
