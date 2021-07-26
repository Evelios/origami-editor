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
      filePath: string option

      (* User Interactivity *)
      hover: Component option
      pressed: Component option
      selectedReferences: Component list
      mousePosition: Point option
      vertexPosition: Point2D option

      (* Viewing Options *)
      axioms: Axiom list
      showVertices: bool
      translation: Translation
      pageSize: Size }
