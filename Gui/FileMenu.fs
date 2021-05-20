namespace Gui

module FileMenu =

    open Avalonia.Controls
    open Avalonia.FuncUI.DSL


    type Msg = OpenFileSettings

    let view dispatch =
        let fileMenu =
            MenuItem.create
            <| [ MenuItem.header "File"
                 MenuItem.viewItems
                 <| [ MenuItem.create
                      <| [ MenuItem.header "File Settings"
                           MenuItem.onClick (fun _ -> dispatch OpenFileSettings) ] ] ]

        Menu.create [ Menu.dock Dock.Top
                      Menu.viewItems [ fileMenu ] ]
