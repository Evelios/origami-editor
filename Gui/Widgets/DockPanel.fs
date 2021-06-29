namespace Gui.Widgets

module DockPanel =
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types
    open Gui

    let private direction dir =
        match dir with
        | Left -> Control.dock Dock.Left
        | Top -> Control.dock Dock.Top
        | Right -> Control.dock Dock.Right
        | Bottom -> Control.dock Dock.Bottom


    let child dir (child: IView<'a>) =
        match dir with
        | Left ->
            Border.create [ direction Left
                            Border.borderThickness (0., 0., Theme.border.thickness, 0.)
                            Border.borderBrush Theme.palette.panelAccent
                            Border.child child ]
        | Top ->
            Border.create [ direction Top
                            Border.borderBrush Theme.palette.panelAccent
                            Border.borderThickness (0., 0., 0., Theme.border.thickness)
                            Border.child child ]
        | Right ->
            Border.create [ direction Right
                            Border.borderBrush Theme.palette.panelAccent
                            Border.borderThickness (Theme.border.thickness, 0., 0., 0.)
                            Border.child child ]
        | Bottom ->
            Border.create [ direction Bottom
                            Border.borderBrush Theme.palette.panelAccent
                            Border.borderThickness (0., Theme.border.thickness, 0., 0.)
                            Border.child child ]
