// Icons can be found here https://materialdesignicons.com/
namespace Gui.Widgets

module Icons =
    open Avalonia.Controls
    open Avalonia.Controls.Shapes
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types

    let iconBase (data: string) : IView<Canvas> =
        Canvas.create [ Canvas.width 24.0
                        Canvas.height 24.0
                        Canvas.children [ Path.create [ Path.fill "white"
                                                        Path.data data ] ] ]


    let adjust =
        iconBase
            "M12,2A10,10 0 0,0 2,12A10,10 0 0,0 12,22A10,10 0 0,0 22,12A10,10 0 0,0 12,2M12,20C7.59,20 4,16.41 4,12C4,7.59 7.59,4 12,4C16.41,4 20,7.59 20,12C20,16.41 16.41,20 12,20M15,12A3,3 0 0,1 12,15A3,3 0 0,1 9,12A3,3 0 0,1 12,9A3,3 0 0,1 15,12Z"
