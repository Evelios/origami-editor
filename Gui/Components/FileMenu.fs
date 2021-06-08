namespace Gui

open Avalonia.FuncUI.Types

module FileMenu =

    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open System.IO
    
    type Msg =
        | NewFile
        | OpenFoldFile
        | OpenExampleFoldFile of string
        | SaveAs

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
