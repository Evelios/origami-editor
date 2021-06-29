namespace Gui.Components

module IconBar =
    open Avalonia.Controls
    open Avalonia.Layout
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types

    open Gui
    open Gui.Widgets
    open Utilities.Extensions

    type Msg = | ToggleShowVertices

    let update msg state : State =
        match msg with
        | ToggleShowVertices ->
            { state with
                  showVertices = not state.showVertices }

    let view (state: State) dispatch =
        let colorToggle isActive =
            if isActive then
                Theme.colors.yellow
            else
                Theme.colors.offWhite

        let iconButtons =
            [ {| icon = Icons.adjust <| colorToggle state.showVertices
                 onClick = Event.handleEvent ToggleShowVertices >> dispatch |} ]
            |> List.map (fun state -> Form.imageButton state :> IView)

        StackPanel.create [ StackPanel.orientation Orientation.Vertical
                            StackPanel.children iconButtons ]
