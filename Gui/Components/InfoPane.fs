namespace Gui.Components


module InfoPane =
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL

    type Msg = DoNothing

    let view =
        StackPanel.create [ StackPanel.children [] ]
