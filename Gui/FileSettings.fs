namespace Gui

open Gui.Widgits
open Utilities


module FileSettings =

    open Avalonia.FuncUI.DSL
    open Avalonia.Controls
    open Avalonia.Layout
    open CreasePattern

    type State = { unit: LengthUnit }

    type Msg = SelectUnit of LengthUnit

    let init = { unit = Unitless }

    let update (msg: Msg) (state: State) : State =
        match msg with
        | SelectUnit unit -> { state with unit = unit }


    // View

    let view (state: State) dispatch =
        StackPanel.create
        <| [ StackPanel.margin 10.
             StackPanel.children
             <| [ TextBlock.create [ TextBlock.text "File Settings" ]
                  DropdownSelection.create
                      {| name = "Units"
                         selected = state.unit
                         onSelected = Msg.SelectUnit >> dispatch |} ] ]
