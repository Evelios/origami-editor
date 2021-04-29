namespace Gui

module Shell =
    open Elmish
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI
    open Avalonia.FuncUI.Components.Hosts
    open Avalonia.FuncUI.Elmish

    type State =
        { creasePatternCanvasState: CreasePatternCanvas.State }

    type Msg = CreasePatternCanvasMsg of CreasePatternCanvas.Msg

    let init =
        { creasePatternCanvasState = CreasePatternCanvas.init }, Cmd.none

    let update (msg: Msg) (state: State) : State * Cmd<_> =
        match msg with
        | CreasePatternCanvasMsg creasePatternCanvasMsg ->
            { state with
                  creasePatternCanvasState =
                      CreasePatternCanvas.update creasePatternCanvasMsg state.creasePatternCanvasState },

            Cmd.none

    let view (state: State) dispatch =
        DockPanel.create [ DockPanel.children [ CreasePatternCanvas.view
                                                    state.creasePatternCanvasState
                                                    (CreasePatternCanvasMsg >> dispatch) ] ]

    type MainWindow() as this =
        inherit HostWindow()

        do
            base.Title <- "Origami Editor"
            base.Width <- 800.0
            base.Height <- 600.0
            base.MinWidth <- 800.0
            base.MinHeight <- 600.0

            //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
            //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

            Elmish.Program.mkProgram (fun () -> init) update view
            |> Program.withHost this
            |> Program.run
