namespace Gui

open Avalonia

open CreasePattern
open Geometry


type Direction =
    | Left
    | Top
    | Right
    | Bottom


type CreasePatternTabState =
    { creasePattern: CreasePattern.CreasePattern
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

type ReferenceFinderTabState =
    { x: float
      y: float
      xInput: string
      yInput: string
      creasePattern: CreasePattern }
