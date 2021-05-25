namespace Gui


module Shell =
    open Elmish
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types
    open Fold
    open Avalonia.FuncUI.Components.Hosts
    open Avalonia.FuncUI.Elmish

    type State =
        { fileSettings: FileSettings.State
          creasePatternCanvasState: CreasePatternCanvas.State }

    type Msg =
        | FileSettingsMsg of FileSettings.Msg
        | CreasePatternCanvasMsg of CreasePatternCanvas.Msg
        | FileMenuMsg of FileMenu.Msg
        | SelectedFoldFilePath of string array

    let init =
        { fileSettings = FileSettings.init
          creasePatternCanvasState = CreasePatternCanvas.init },
        Cmd.none

    let update (msg: Msg) (state: State) (window: HostWindow) : State * Cmd<_> =
        match msg with
        (* Component Messages *)
        | FileSettingsMsg fileSettingsMsg ->
            { state with
                  fileSettings = FileSettings.update fileSettingsMsg state.fileSettings },
            Cmd.none

        | CreasePatternCanvasMsg creasePatternCanvasMsg ->
            { state with
                  creasePatternCanvasState =
                      CreasePatternCanvas.update creasePatternCanvasMsg state.creasePatternCanvasState },
            Cmd.none

        | FileMenuMsg fileMenuMsg ->
            match fileMenuMsg with
            | FileMenu.Msg.OpenFoldFile ->
                let dialog =
                    Dialogs.getFileDialog "Fold File" Fold.extensions

                let showDialog window =
                    dialog.ShowAsync(window) |> Async.AwaitTask

                state, Cmd.OfAsync.perform showDialog window SelectedFoldFilePath
            | FileMenu.Msg.OpenFileSettings -> state, Cmd.none

        (* Global Messages*)
        | SelectedFoldFilePath foldFilePaths ->
            match FileLoader.loadFoldFile foldFilePaths.[0] with
            | Ok foldContents -> state, Cmd.none
            | Error _ -> state, Cmd.none


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
            this.HasSystemDecorations <- true

            let updateWithServices (msg: Msg) (state: State) = update msg state this

            Program.mkProgram (fun () -> init) updateWithServices view
            |> Program.withHost this
#if DEBUG
            |> Program.withConsoleTrace
#endif
            |> Program.run
