namespace Gui.Tabs.ReferenceFinderTab

open Geometry

module ReferenceFinderTab =
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types
    open Avalonia.Layout

    open CreasePattern
    open Gui
    open Gui.Widgets
    open Utilities.Extensions

    type Msg =
        | ChangeXInput of string
        | ChangeYInput of string
        | RunReferenceFinder

    let init : ReferenceFinderTabState =
        let x, y = 0.33, 0.33

        { x = x
          y = y
          xInput = x.ToString()
          yInput = y.ToString()
          referenceFinder = ReferenceFinder.init
          creasePattern = CreasePattern.create }

    let update (msg: Msg) (state: ReferenceFinderTabState) : ReferenceFinderTabState =
        match msg with
        | ChangeXInput xString ->
            match String.parseFloat xString with
            | Some newX -> { state with x = newX }
            | None -> state
        | ChangeYInput yString ->
            match String.parseFloat yString with
            | Some newY -> { state with y = newY }
            | None -> state
        | RunReferenceFinder ->
            { state with
                  creasePattern = ReferenceFinder.bestFoldSequenceTo (Point2D.xy state.x state.y) state.referenceFinder }

    let private toolBar (state: ReferenceFinderTabState) dispatch =
        let coordinates : IView list =
            [ Form.textItem
                {| name = "X"
                   value = state.xInput
                   onSelected = ChangeXInput >> dispatch
                   labelPlacement = Orientation.Horizontal |}
              Form.textItem
                  {| name = "Y"
                     value = state.yInput
                     onSelected = ChangeYInput >> dispatch
                     labelPlacement = Orientation.Horizontal |} ]

        let actionButton =
            Button.create [ Button.onClick (fun _ -> dispatch RunReferenceFinder)
                            Button.content "Find fold sequences" ]

        StackPanel.create
        <| [ StackPanel.orientation Orientation.Horizontal
             StackPanel.background Theme.palette.panelBackground
             StackPanel.children (coordinates @ [ actionButton ]) ]

    let private foldSequences =
        let title = Text.h1 "Fold Sequences"

        let steps : IView =
            [ "Crease fold from top to bottom"
              "Crease left to right" ]
            |> Text.numberedList

        StackPanel.create
        <| [ StackPanel.orientation Orientation.Vertical
             StackPanel.background Theme.palette.panelBackground
             StackPanel.children [ title; steps ] ]


    let private creasePattern cp =
        DockPanel.create
        <| [ DockPanel.background Theme.palette.canvasBackdrop
             DockPanel.children [ CreasePatternDrawing.create cp ] ]

    let view (state: ReferenceFinderTabState) dispatch =


        DockPanel.create
        <| [ DockPanel.children [ View.withAttr (StackPanel.dock Dock.Top) (toolBar state dispatch)
                                  View.withAttr (StackPanel.dock Dock.Right) foldSequences
                                  creasePattern state.creasePattern ] ]
