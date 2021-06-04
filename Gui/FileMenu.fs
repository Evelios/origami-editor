namespace Gui

open Avalonia.FuncUI.Types

module FileMenu =

    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open System.IO




    type Msg =
        | OpenFoldFile
        | OpenExampleFoldFile of string
        | OpenFileSettings



    let view dispatch =
        let exampleFiles : IView list =
            let examplesDirectory =
                Path.Join(__SOURCE_DIRECTORY__, "..", "Assets", "Examples")

            Directory.GetFiles(examplesDirectory, "*.fold")
            |> Array.map
                (fun path ->
                    MenuItem.create [ MenuItem.header (Path.GetFileNameWithoutExtension(path))
                                      MenuItem.onClick (fun _ -> dispatch <| OpenExampleFoldFile path) ]
                    :> IView)
            |> Array.toList

        let menuOptions : IView list =
            [ MenuItem.create
              <| [ MenuItem.header "Open"
                   MenuItem.onClick (fun _ -> dispatch OpenFoldFile) ]
              MenuItem.create
              <| [ MenuItem.header "Open Example"
                   MenuItem.viewItems exampleFiles ]
              MenuItem.create
              <| [ MenuItem.header "File Settings"
                   MenuItem.onClick (fun _ -> dispatch OpenFileSettings) ] ]

        let fileMenu =
            MenuItem.create
            <| [ MenuItem.header "File"
                 MenuItem.viewItems menuOptions ]

        Menu.create [ Menu.dock Dock.Top
                      Menu.viewItems [ fileMenu ] ]
