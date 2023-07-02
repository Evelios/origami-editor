namespace Gui

open Avalonia

open Origami

open Math.Geometry
open Math.Units

type Direction =
    | Left
    | Top
    | Right
    | Bottom


type CreasePatternTabState =
    { creasePattern: CreasePattern
      creasePatternPreview: CreasePattern
      filePath: string option

      (* User Interactivity *)
      hover: GraphElement option
      pressed: GraphElement option
      selectedReferences: GraphElement list
      mousePosition: Point option
      vertexPosition: Point2D<Meters, OrigamiCoordinates> option

      (* Viewing Options *)
      axioms: Axiom list
      showVertices: bool
      translation: Translation
      pageSize: Size2D<Meters> }

type ReferenceFinderTabState =
    { x: float
      y: float
      xInput: string
      yInput: string
      referenceFinder: ReferenceFinder.LookupTable
      solutions: ReferenceFinderSolution array
      hoveredStep: int option
      activeSolution: int
      hoveredSolution: int option }
