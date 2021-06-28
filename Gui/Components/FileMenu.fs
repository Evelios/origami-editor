namespace Gui

module FileMenu =

    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types
    open Elmish
    open System.IO

    open Fold
    open Gui.Widgets

    type External =
        | CreateNewFile
        | SelectedFoldFilePath of string array
        | LoadFoldFile of string
        | SaveFoldFileToPath of string
        | DoNothing

    type Msg =
        | SendExternal of External
        | NewFile
        | OpenFoldFile
        | OpenExampleFoldFile of string
        | SaveAs

    let update msg window : Cmd<Msg> * External =

        match msg with
        | NewFile -> Cmd.none, External.CreateNewFile

        | OpenFoldFile ->
            let fileDialogTask =
                Dialogs.openFileDialogTask "Fold File" Fold.extensions window

            Cmd.OfAsync.perform fileDialogTask () (SelectedFoldFilePath >> SendExternal), DoNothing

        | OpenExampleFoldFile path -> Cmd.ofMsg <| (LoadFoldFile path |> SendExternal), DoNothing

        | SaveAs ->
            let fileDialogTask =
                Dialogs.saveFileDialogTask "Fold File" Fold.extensions window

            Cmd.OfAsync.perform fileDialogTask () (SaveFoldFileToPath >> SendExternal), DoNothing

        | SendExternal external -> Cmd.none, external

    let view dispatch =
        let exampleFiles : IView list =
            let examplesDirectory =
                Path.Join(__SOURCE_DIRECTORY__, "..", "..", "Assets", "Examples")

            Directory.GetFiles(examplesDirectory, "*.fold")
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
                 MenuItem.viewItems menuOptions ]

        Menu.create [ Menu.dock Dock.Top
                      Menu.viewItems [ fileMenu ] ]
