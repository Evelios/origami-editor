namespace Gui.Tabs.ReferenceFinderTab

module ReferenceFinderTab =
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types
    open Avalonia.Layout

    open CreasePattern
    open Gui
    open Gui.Widgets

    type Msg =
        | ChangeXInput of string
        | ChangeYInput of string

    let init : ReferenceFinderTabState =
        { x = 0.
          y = 0.
          xInput = "0"
          yInput = "0"
          creasePattern = CreasePattern.create }

    let update (msg: Msg) (state: ReferenceFinderTabState) : ReferenceFinderTabState =
        match msg with
        | ChangeXInput xString -> state
        | ChangeYInput yString -> state

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

        let toolBar =
            StackPanel.create
            <| [ StackPanel.orientation Orientation.Horizontal
                 StackPanel.background Theme.palette.panelBackground
                 StackPanel.children coordinates ]

        let creasePattern =
            DockPanel.create
            <| [ DockPanel.background Theme.palette.canvasBackdrop
                 DockPanel.children [ CreasePatternDrawing.create state.creasePattern ] ]

        StackPanel.create
        <| [ StackPanel.orientation Orientation.Vertical
             StackPanel.children [ toolBar
                                   creasePattern ] ]
