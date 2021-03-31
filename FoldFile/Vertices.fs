namespace Fold

type Vertices =
    { coords: Vertex list
      vertices: int list
      faces: int list list }

module Vertices =

    let Create a: Vertices = a

    let Empty: Vertices =
        { coords = []
          vertices = []
          faces = [] }
