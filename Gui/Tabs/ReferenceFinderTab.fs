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


    let view (state: ReferenceFinderTabState) dispatch =
        let coordinates : IView list =
            [ Form.textItem
                {| name = "X"
                   value = state.xInput
                   onSelected = ChangeXInput >> dispatch |}
              Form.textItem
                  {| name = "Y"
                     value = state.yInput
                     onSelected = ChangeYInput >> dispatch |} ]

        let actionButton =
            Button.create [ Button.onClick (fun _ -> dispatch RunReferenceFinder)
                            Button.content "Find fold sequences" ]


        let toolBar =
            StackPanel.create
            <| [ StackPanel.orientation Orientation.Horizontal
                 StackPanel.background Theme.palette.panelBackground
                 StackPanel.children (coordinates @ [ actionButton ]) ]

        let creasePattern =
            DockPanel.create
            <| [ DockPanel.background Theme.palette.canvasBackdrop
                 DockPanel.children [ CreasePatternDrawing.create state.creasePattern ] ]

        DockPanel.create
        <| [ DockPanel.children [ View.withAttr (StackPanel.dock Dock.Top) toolBar
                                  creasePattern ] ]
