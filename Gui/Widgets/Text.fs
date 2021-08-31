namespace Gui.Widgets

module Text =
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types
    open Avalonia.Layout

    open Gui

    /// Generic builder for text based elements
    let private textBlock
        (options: {| text: string
                     margin: float
                     fontSize: float |})
        attrs
        =
        TextBlock.create
        <| [ TextBlock.text options.text
             TextBlock.margin options.margin
             TextBlock.fontSize options.fontSize ]
           @ attrs
        :> IView

    let paragraph text =
        textBlock
            {| text = text
               margin = Theme.spacing.small
               fontSize = Theme.font.normal |}

    let h1 text =
        textBlock
            {| text = text
               margin = Theme.spacing.medium
               fontSize = Theme.font.h1 |}

    let h2 text =
        textBlock
            {| text = text
               margin = Theme.spacing.medium
               fontSize = Theme.font.h2 |}

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
