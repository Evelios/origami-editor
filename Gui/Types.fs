namespace Gui

open Fold
open CreasePattern
open Utilities.Collections

type Direction =
    | Left
    | Top
    | Right
    | Bottom

type Selectable =
    | SelectedVertex of Vertex
    | SelectedNone

type State =
    { frame: Frame
      showVertices: bool
      hover: Selectable
      selected: Selectable
      translation: Translation
      pageSize: Size
      filePath: string option }
