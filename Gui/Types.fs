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

type Selectable =
    | SelectedEdge of Edge
    | SelectedVertex of Vertex
    | SelectedNone

type State =
    { frame: Frame
      showVertices: bool
      hover: Selectable
      selected: Selectable
      translation: Translation
      mousePosition: Point option
      vertexPosition: Vertex option
      pageSize: Size
      filePath: string option }
