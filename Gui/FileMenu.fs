namespace Gui

open Avalonia.FuncUI.Types

module FileMenu =

    open Avalonia.Controls
    open Avalonia.FuncUI.DSL


    type Msg =
        | OpenFoldFile
        | OpenFileSettings

    let view dispatch =

        let menuOptions : IView list =
            [ MenuItem.create
              <| [ MenuItem.header "Open Fold File"
                   MenuItem.onClick (fun _ -> dispatch OpenFoldFile) ]
              MenuItem.create
              <| [ MenuItem.header "File Settings"
                   MenuItem.onClick (fun _ -> dispatch OpenFileSettings) ] ]

        let fileMenu =
            MenuItem.create
            <| [ MenuItem.header "File"
                 MenuItem.viewItems menuOptions ]

        Menu.create [ Menu.dock Dock.Top
                      Menu.viewItems [ fileMenu ] ]
