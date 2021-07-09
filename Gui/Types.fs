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

type Selected =
    | SelectedNone
    | SelectedOne of Component
    | SelectedTwo of Component * Component

type State =
    { frame: Frame
      showVertices: bool
      hover: Component option
      selected: Selected
      translation: Translation
      mousePosition: Point option
      vertexPosition: Point2D option
      pageSize: Size
      filePath: string option }
