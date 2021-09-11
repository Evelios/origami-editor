namespace Gui.Tabs.CreasePatternTab


module FileMenu =

    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types
    open Elmish
    open System
    open System.IO

    open Fold
    open CreasePattern
    open Gui.Widgets
    open Gui

    type External =
        | FoldFileLoaded
        | DoNothing

    type Msg =
        | NewFile
        | OpenFoldFile
        | OpenExampleFoldFile of string
        | LoadFoldFiles of string array
        | LoadFoldFile of string
        | SaveAs
        | SaveFoldFileToPath of string
        | SavedFile
        | ErrorSavingFile of exn

    let update msg state window : CreasePatternTabState * Cmd<Msg> * External =

        match msg with
        | NewFile ->
            { state with
                  creasePattern = CreasePattern.create },
            Cmd.none,
            DoNothing
        | OpenFoldFile ->
            let fileDialogTask =
                Dialogs.openFileDialogTask "Fold File" Fold.extensions window

            state, Cmd.OfAsync.perform fileDialogTask () LoadFoldFiles, DoNothing

        | LoadFoldFiles paths ->
            match Array.tryHead paths with
            | Some path -> state, Cmd.ofMsg (LoadFoldFile path), DoNothing
            | None -> state, Cmd.none, DoNothing

        | OpenExampleFoldFile path -> state, Cmd.ofMsg (LoadFoldFile path), DoNothing

        | LoadFoldFile path ->
            match FileLoader.loadFoldFile path with
            | Ok foldContents ->
                { state with
                      creasePattern = CreasePattern.fromFoldFrame foldContents.KeyFrame },
                Cmd.none,
                FoldFileLoaded

            | Error error ->
                printfn $"An error occured loading fold file: {path}{Environment.NewLine}{error}"
                state, Cmd.none, DoNothing

        | SaveAs ->
            let fileDialogTask =
                Dialogs.saveFileDialogTask "Fold File" Fold.extensions window

            state, Cmd.OfAsync.perform fileDialogTask () SaveFoldFileToPath, DoNothing

        | SaveFoldFileToPath path ->
            let foldText =
                Fold.empty
                |> Fold.setKeyFrame (CreasePattern.toFoldFrame state.creasePattern)
                |> Fold.toJson

            let writeToFile () =
                async { File.WriteAllText(path, foldText) }

            state, Cmd.OfAsync.either writeToFile () (fun () -> SavedFile) ErrorSavingFile, DoNothing

        | SavedFile -> state, Cmd.none, DoNothing
        | ErrorSavingFile exn -> state, Cmd.none, DoNothing

    let view dispatch =
        let exampleFiles : IView list =
            let examplesDirectory =
                Path.Join(__SOURCE_DIRECTORY__, "..", "..", "Assets", "Examples")

            let exampleFiles =
                try
                    Directory.GetFiles(examplesDirectory, "*.fold")
                with _ -> [||]


            exampleFiles
            |> Array.map
                (fun path ->
                    MenuItem.create [ MenuItem.header (Path.GetFileNameWithoutExtension(path))
                                      MenuItem.onClick (fun _ -> dispatch <| OpenExampleFoldFile path) ]
                    :> IView)
            |> Array.toList

        let menuOptions : IView list =
            [ MenuItem.create
              <| [ MenuItem.header "New File"
                   MenuItem.onClick (fun _ -> dispatch NewFile) ]
              MenuItem.create
              <| [ MenuItem.header "Open"
                   MenuItem.onClick (fun _ -> dispatch OpenFoldFile) ]
              MenuItem.create
              <| [ MenuItem.header "Open Example"
                   MenuItem.viewItems exampleFiles ]
              MenuItem.create
              <| [ MenuItem.header "Save As"
                   MenuItem.onClick (fun _ -> dispatch SaveAs) ] ]

        let fileMenu =
            MenuItem.create
            <| [ MenuItem.header "File"
                 Menu.padding Theme.spacing.small
                 MenuItem.viewItems menuOptions ]

        let menu =
            Menu.create [ Menu.viewItems [ fileMenu ] ]


        Border.create [ Border.borderBrush Theme.palette.panelAccent
                        Border.borderThickness (0., 0., 0., Theme.border.thickness)
                        Border.child menu ]
