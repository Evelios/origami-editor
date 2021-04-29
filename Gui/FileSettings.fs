namespace Gui


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


    // View Functions



    // View

    let view (state: State) dispatch =
        let unitComboBox =
            let unitName unit : string =
                match unit with
                | LengthUnit.Unitless -> "Unitless"
                | LengthUnit.Pixels -> "Pixels"

            StackPanel.create
            <| [ StackPanel.orientation Orientation.Horizontal
                 StackPanel.children
                 <| [ TextBlock.create [ TextBlock.text "Units" ]
                      ComboBox.create
                      <| [ ComboBox.dataItems (List.map unitName LengthUnit.all)
                           ComboBox.selectedItem (unitName state.unit)
                           ComboBox.onSelectedItemChanged (
                               tryUnbox
                               >> Option.iter (Msg.SelectUnit >> dispatch)
                           ) ] ] ]

        StackPanel.create
        <| [ StackPanel.margin 10.
             StackPanel.children
             <| [ TextBlock.create [ TextBlock.text "File Settings" ]
                  unitComboBox ] ]
