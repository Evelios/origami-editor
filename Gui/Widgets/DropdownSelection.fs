namespace Gui.Widgets

open Avalonia.FuncUI.Types


module DropdownSelection =

    open Avalonia.FuncUI.DSL
    open Avalonia.Controls
    open Avalonia.Layout
    open Utilities.Extensions

    let create
        (state: {| name: string
                   selected: 'a
                   onSelected: 'a -> unit |})
        : IView<StackPanel> =
        StackPanel.create
        <| [ StackPanel.orientation Orientation.Horizontal
             StackPanel.children
             <| [ TextBlock.create [ TextBlock.text state.name ]
                  ComboBox.create
                  <| [ ComboBox.dataItems (Seq.map DiscriminatedUnion.toString DiscriminatedUnion.allCases<'a>)
                       ComboBox.selectedItem (DiscriminatedUnion.toString state.selected)
                       ComboBox.onSelectedItemChanged (tryUnbox >> Option.iter state.onSelected) ] ] ]
