namespace Gui

open Avalonia.FuncUI.Types
open Avalonia.Layout

module Shell =
    open Elmish
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI
    open Avalonia.FuncUI.Components.Hosts
    open Avalonia.FuncUI.Elmish

    type State =
        { fileSettings: FileSettings.State
          creasePatternCanvasState: CreasePatternCanvas.State }

    type Msg =
        | FileSettingsMsg of FileSettings.Msg
        | CreasePatternCanvasMsg of CreasePatternCanvas.Msg
        | FileMenuMsg of FileMenu.Msg

    let init =
        { fileSettings = FileSettings.init
          creasePatternCanvasState = CreasePatternCanvas.init },
        Cmd.none

    let update (msg: Msg) (state: State) : State * Cmd<_> =
        match msg with
        | FileSettingsMsg fileSettingsMsg ->
            { state with
                  fileSettings = FileSettings.update fileSettingsMsg state.fileSettings }
        | CreasePatternCanvasMsg creasePatternCanvasMsg ->
            { state with
                  creasePatternCanvasState =
                      CreasePatternCanvas.update creasePatternCanvasMsg state.creasePatternCanvasState }
        | FileMenuMsg fileMenuMsg -> state
        ,

        Cmd.none

    let view (state: State) dispatch =
        let body =
            let children : IView list =
                [ FileSettings.view state.fileSettings (FileSettingsMsg >> dispatch)
                  CreasePatternCanvas.view state.creasePatternCanvasState (CreasePatternCanvasMsg >> dispatch) ]

            DockPanel.create [ DockPanel.children children ]

        DockPanel.create
        <| [ DockPanel.children
             <| [ FileMenu.view (FileMenuMsg >> dispatch)
                  body ] ]

    type MainWindow() as this =
        inherit HostWindow()

        do
            base.Title <- "Origami Editor"
            base.Width <- 800.0
            base.Height <- 600.0
            base.MinWidth <- 800.0
            base.MinHeight <- 600.0

            //            this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
//            this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

            Elmish.Program.mkProgram (fun () -> init) update view
            |> Program.withHost this
            |> Program.run
