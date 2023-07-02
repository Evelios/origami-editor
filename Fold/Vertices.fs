namespace Fold

open Math.Geometry
open Math.Units

type Vertices<'Coordinates> =
    { Coordinates: Point2D<Meters, 'Coordinates> list
      Vertices: int list
      Faces: int list list }
    
module Vertices =
    let create a : Vertices<'Coordinates> = a

    let empty : Vertices<'Coordinates> =
        { Coordinates = []
          Vertices = []
          Faces = [] }
