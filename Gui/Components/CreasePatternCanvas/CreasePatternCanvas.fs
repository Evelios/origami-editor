namespace Gui.Components.CreasePatternCanvas

module CreasePatternCanvas =

    open Avalonia
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types
    open Avalonia.Interactivity
    open Avalonia.VisualTree

    open CreasePattern
    open Fold
    open Utilities.Collections
    open Utilities.Extensions
    open Gui.Widgets
    open Gui

    type Selectable =
        | SelectedVertex of Vertex
        | SelectedNone

    type State =
        { showVertices: bool
          hover: Selectable
          selected: Selectable
          translation: Translation
          pageSize: Size }

    type Msg =
        | MouseMove of Point
        | MouseClicked
        | ToggleShowVertices of RoutedEventArgs

    let theme =
        {| maxLength = 500.
           pointerCloseDistance = 20. |}

    let init (creasePattern: CreasePattern) =
        let translation =
            Translation.create creasePattern theme.maxLength

        { showVertices = false
          hover = SelectedNone
          selected = SelectedNone
          translation = translation
          pageSize = translation.pageSize }

    let update msg state creasePattern =
        match msg with
        | MouseClicked -> { state with selected = state.hover }

        /// Point input is currently offset from the crease pattern because of the added buffer on the outside
        /// of the canvas
        | MouseMove mousePoint ->
            let vertexWithin =
                CreasePattern.pointWithin
                    (theme.pointerCloseDistance
                     / (max state.translation.xRatio state.translation.yRatio))
                    (Translation.pointToVertex state.translation mousePoint)
                    creasePattern

            match vertexWithin with
            | Some vertex ->
                { state with
                      hover = SelectedVertex vertex }
            | None -> { state with hover = SelectedNone }


        | ToggleShowVertices _ ->
            { state with
                  showVertices = not state.showVertices }


    (* Drawing *)

    let canvas (state: State) (creasePattern: CreasePattern) dispatch : IView =
        let edgeLines =
            List.map (CreasePatternComponents.edgeLine state.translation) (CreasePattern.edges creasePattern)

        let vertexPoints =
            List.map (CreasePatternComponents.vertexDefault state.translation) (CreasePattern.vertices creasePattern)

        let hoverElement =
            match state.hover with
            | SelectedVertex vertex ->
                Some
                <| CreasePatternComponents.vertexHovered state.translation vertex
            | SelectedNone -> None

        let selectedElement =
            match state.selected with
            | SelectedVertex vertex ->
                Some
                <| CreasePatternComponents.vertexSelected state.translation vertex
            | SelectedNone -> None

        // This could be a slow point because of appending to the end of the list
        // If this gets too slow, cons to the beginning and reverse the list to maintain readability
        let canvasElements =
            edgeLines
            |> List.concatIf state.showVertices vertexPoints
            |> List.appendWhenSome hoverElement
            |> List.appendWhenSome selectedElement

        Canvas.create [ Canvas.height state.pageSize.height
                        Canvas.width state.pageSize.width
                        Canvas.background Theme.colors.foreground
                        Canvas.children canvasElements
                        Canvas.onPointerReleased (fun _ -> dispatch Msg.MouseClicked)
                        Canvas.onPointerMoved
                            (fun event ->
                                event.GetPosition(event.Source :?> IVisual)
                                |> Msg.MouseMove
                                |> dispatch) ]
        :> IView

    let buttons dispatch =
        let iconButtons : IView list =
            [ Form.imageButton
                  {| icon = Icons.adjust
                     onClick = Msg.ToggleShowVertices >> dispatch |} ]

        StackPanel.create [ StackPanel.children iconButtons ]

    let view (state: State) (creasePattern: CreasePattern) dispatch =
        DockPanel.create
        <| [ DockPanel.background Theme.colors.background
             DockPanel.children [ buttons dispatch
                                  canvas state creasePattern dispatch ] ]
