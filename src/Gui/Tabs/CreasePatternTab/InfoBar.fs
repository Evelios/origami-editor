namespace Gui.Tabs.CreasePatternTab


module InfoBar =
    open Avalonia.FuncUI.DSL

    open Gui

    type Msg = DoNothing

    let update (msg: Msg) (state: CreasePatternTabState) : CreasePatternTabState =
        match msg with
        | DoNothing -> state

    let view (state: CreasePatternTabState) dispatch =

        StackPanel.create []
