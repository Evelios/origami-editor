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
    { (* Main Data *)
      creasePattern: CreasePattern
      filePath: string option

      (* User Interactivity *)
      hover: Component option
      pressed: Component option
      selected: Component option
      selectedReferences: Component list
      mousePosition: Point option
      vertexPosition: Point2D option

      (* Viewing Options *)
      showVertices: bool
      translation: Translation
      pageSize: Size }
