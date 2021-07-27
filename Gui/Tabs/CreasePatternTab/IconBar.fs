namespace Gui.Tabs.CreasePatternTab

module IconBar =
    open Avalonia.Controls
    open Avalonia.Layout
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types

    open Gui
    open Gui.Widgets
    open Utilities.Extensions

    type Msg =
        | ToggleShowVertices
        | ToggleFirstAxiom
        | ToggleSecondAxiom
        | ToggleThirdAxiom

    let update msg state : CreasePatternTabState =
        match msg with
        | ToggleShowVertices ->
            { state with
                  showVertices = not state.showVertices }
        | ToggleFirstAxiom -> state
        | ToggleSecondAxiom -> state
        | ToggleThirdAxiom -> state

    let view (state: CreasePatternTabState) dispatch =
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
