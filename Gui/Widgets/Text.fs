namespace Gui.Widgets

module Text =
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types
    open Avalonia.Layout

    open Gui


    let h1 text attrs =
        TextBlock.create
        <| [ TextBlock.text text
             TextBlock.margin Theme.spacing.medium
             TextBlock.fontSize Theme.font.h1 ]
           @ attrs
        :> IView

    let h2 text attrs =
        TextBlock.create
        <| [ TextBlock.text text
             TextBlock.margin Theme.spacing.medium
             TextBlock.fontSize Theme.font.h2 ]
           @ attrs
        :> IView

    /// Create a widget of numbered items.
    /// Eg.
    ///     1. First
    ///     2. Second
    ///     3. Third
    let numberedList items =
        let numberedItem index text =
            TextBlock.create
            <| [ TextBlock.text $"{index + 1}. {text}"
                 TextBlock.margin Theme.spacing.small
                 TextBlock.textWrapping Avalonia.Media.TextWrapping.Wrap ]
            :> IView

        StackPanel.create
        <| [ StackPanel.orientation Orientation.Vertical
             StackPanel.children (List.mapi numberedItem items)
             StackPanel.margin Theme.spacing.medium ]
        :> IView
