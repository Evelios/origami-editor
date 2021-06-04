namespace Gui


module Shell =
    open System
    open System.IO

    open Elmish
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types
    open Avalonia.FuncUI.Components.Hosts
    open Avalonia.FuncUI.Elmish

    open CreasePattern
    open Fold

    type State =
        { fileSettings: FileSettings.State
          frame: CreasePattern.Frame }

    type Msg =
        (* Component Messages *)
        | FileSettingsMsg of FileSettings.Msg
        | CreasePatternCanvasMsg of CreasePatternCanvas.Msg
        | FileMenuMsg of FileMenu.Msg

        (* Global Messages *)
        | SelectedFoldFilePath of string array
        | LoadFoldFile of string
        | UpdateTitle of string

    let title = "Origami Editor"

    let init =
        { fileSettings = FileSettings.init
          frame = CreasePattern.Frame.create },
        Cmd.none

    let update (msg: Msg) (state: State) (window: HostWindow) : State * Cmd<_> =
        let noUpdate = state, Cmd.none

        match msg with

        (* Component Messages *)
        | FileSettingsMsg fileSettingsMsg ->
            { state with
                  fileSettings = FileSettings.update fileSettingsMsg state.fileSettings },
            Cmd.none

        | FileMenuMsg fileMenuMsg ->
            match fileMenuMsg with
            | FileMenu.Msg.OpenFoldFile ->
                let dialog =
                    Dialogs.getFileDialog "Fold File" Fold.extensions

                let showDialog window =
                    dialog.ShowAsync(window) |> Async.AwaitTask

                state, Cmd.OfAsync.perform showDialog window SelectedFoldFilePath

            | FileMenu.Msg.OpenExampleFoldFile path -> state, Cmd.ofMsg <| LoadFoldFile path
            | FileMenu.Msg.OpenFileSettings -> state, Cmd.none


        | CreasePatternCanvasMsg _ -> noUpdate

        (* Global Messages*)
        | UpdateTitle newTitle ->
            window.Title <- $"{title} - {newTitle}"
            noUpdate


        | SelectedFoldFilePath foldFilePaths ->
            if foldFilePaths = null then
                noUpdate
            else
                match Array.tryHead foldFilePaths with
                | Some foldPath -> state, Cmd.ofMsg <| LoadFoldFile foldPath
                | _ -> noUpdate

        | LoadFoldFile foldPath ->
            match FileLoader.loadFoldFile foldPath with
            | Ok foldContents ->
                { state with
                      frame = Frame.fromFoldFrame foldContents.keyFrame },
                Cmd.ofMsg
                <| UpdateTitle(Path.GetFileNameWithoutExtension foldPath)

            | Error error ->
                printfn $"An error occured loading fold file: {foldPath}{Environment.NewLine}{error}"
                noUpdate




    let view (state: State) dispatch =
        let body =
            let children : IView list =
                [ FileSettings.view state.fileSettings (FileSettingsMsg >> dispatch)
                  CreasePatternCanvas.view state.frame.creasePattern (CreasePatternCanvasMsg >> dispatch) ]

            DockPanel.create [ DockPanel.children children ]

        DockPanel.create
        <| [ DockPanel.background Theme.colors.backgroundDark
             DockPanel.children
             <| [ FileMenu.view (FileMenuMsg >> dispatch)
                  body ] ]

    type MainWindow() as this =
        inherit HostWindow()

        do
            base.Title <- title
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
