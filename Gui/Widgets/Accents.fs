namespace Gui.Widgets

module Accents =
    open Avalonia.FuncUI.Types
    open Avalonia.FuncUI.DSL
    open Avalonia.Controls

    open Gui

    let selectable selected attr (child: IView<'a>) =
        if selected then
            Border.create (
                [ Border.borderThickness Theme.border.thickness
                  Border.padding Theme.spacing.small
                  Border.borderBrush Theme.palette.primary
                  Border.background (Color.withAlpha 0.25 Theme.palette.primary)
                  Border.cornerRadius Theme.border.cornerRadius
                  Border.child child ]
                @ attr
            )
        else
            Border.create (
                [ Border.padding (Theme.spacing.small + Theme.border.thickness)
                  Border.child child ]
                @ attr
            )
