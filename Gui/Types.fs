namespace Gui

open Avalonia

open Origami
open Math.Units
open Math.Geometry


/// The coordinate system being used within the Gui for manipulating crease patterns.
type UserSpace = UserSpace

type Direction =
    | Left
    | Top
    | Right
    | Bottom


type CreasePatternTabState =
    { creasePattern: CreasePattern<UserSpace>
      creasePatternPreview: CreasePattern<UserSpace>
      filePath: string option

      (* User Interactivity *)
      hover: GraphElement<UserSpace> option
      pressed: GraphElement<UserSpace> option
      selectedReferences: GraphElement<UserSpace> list
      mousePosition: Point option
      vertexPosition: Point2D<Meters, UserSpace> option

      (* Viewing Options *)
      axioms: Axiom list
      showVertices: bool
      translation: Translation
      pageSize: Size2D<Meters> }

type ReferenceFinderTabState =
    { x: Length
      y: Length
      xInput: string
      yInput: string
      referenceFinder: ReferenceFinder<UserSpace>
      solutions: ReferenceFinderSolution<UserSpace> array
      hoveredStep: int option
      activeSolution: int
      hoveredSolution: int option }
