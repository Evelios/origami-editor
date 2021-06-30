namespace Gui.Components



module InfoBar =
    open Avalonia.FuncUI.DSL

    open Gui

    type Msg = DoNothing

    let update (msg: Msg) (state: State) : State =
        match msg with
        | DoNothing -> state

    let view (state: State) dispatch =

        StackPanel.create []
