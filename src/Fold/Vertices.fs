namespace Fold

open Geometry

type Vertices =
    { Coordinates: Point2D list
      Vertices: int list
      Faces: int list list }
    
module Vertices =
    let create a : Vertices = a

    let empty : Vertices =
        { Coordinates = []
          Vertices = []
          Faces = [] }
