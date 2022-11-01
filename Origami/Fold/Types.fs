namespace Origami.Fold

open Math.Geometry
open Math.Units

open Origami

type Id<'Unit> = Id of int

type Vertices<'Coordinates> =
    { Coordinates: Point2D<Meters, 'Coordinates> list
      Vertices: int list
      Faces: int list list }


type Edges =
    { Vertices: (int * int) list
      Faces: (int * int option) list
      Assignment: EdgeAssignment list
      FoldAngle: float list // should be angle
      Length: float list
      Orders: (int (*edge id*)  * int (*edge id*)  * int (*order*) ) list }

type Faces =
    { Vertices: int list list
      Edges: int list list
      Orders: (int (*face id*)  * int (*face id*)  * int (*order*) ) list }

type FrameClass =
    | CreasePattern
    | FoldedForm
    | Graph
    | Linkage

type FrameAttribute =
    | Geo2D
    | Geo3D
    | Abstract
    | Orientable
    | Manifold
    | NonManifold
    | NonOrientable
    | SelfTouching
    | NonSelfTouching
    | NonSelfIntersecting

type LengthUnit =
    | Unitless
    | Inches
    | Points
    | Meters
    | Centimeters
    | Millimeters
    | Micrometers
    | Nanometers

type Frame<'Coordinates> =
    { Author: string
      Title: string
      Description: string
      Classes: FrameClass Set
      Attributes: FrameAttribute Set
      Unit: LengthUnit
      Vertices: Vertices<'Coordinates>
      Edges: Edges
      Faces: Faces }

type FileClass =
    | SingleModel
    | MultiModel
    | Animation
    | Diagrams

type Fold<'Coordinates> =
    { Spec: int
      Creator: string
      Author: string
      Title: string
      Description: string
      Classes: FileClass Set
      KeyFrame: Frame<'Coordinates>
      Frames: Frame<'Coordinates> list }
