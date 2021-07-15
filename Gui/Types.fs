namespace Gui

open Avalonia
open CreasePattern
open Utilities.Collections
open Geometry

type Direction =
    | Left
    | Top
    | Right
    | Bottom

type State =
    { creasePattern: CreasePattern
      showVertices: bool
      hover: Component option
      selected: Component option
      selectedReferences: Component list
      translation: Translation
      mousePosition: Point option
      vertexPosition: Point2D option
      pageSize: Size
      filePath: string option }
