namespace Gui.Components.CreasePatternCanvas

module CreasePatternCanvas =

    open Avalonia
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types
    open Avalonia.Interactivity
    open Avalonia.FuncUI.Components.Hosts

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
        | CreateEdge of Vertex * Vertex

    let canvasName = "Crease Pattern Canvas"

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

    let rec update msg state shared (window: HostWindow) : {| state: State; shared: SharedState |} =
        let updateState newState = {| state = newState; shared = shared |}
        let updateShared newShared = {| state = state; shared = newShared |}

        match msg with
        (* Crease Pattern Modifications *)
        | CreateEdge (start, finish) ->
            let edge =
                Edge.create
                    { start = start
                      finish = finish
                      assignment = EdgeAssignment.Unassigned }

            updateShared
                { shared with
                      frame =
                          { shared.frame with
                                creasePattern = CreasePattern.addEdge edge shared.frame.creasePattern } }

        (* User Actions *)
        | MouseClicked ->
            match (state.selected, state.hover) with
            | SelectedVertex selectedVertex, SelectedVertex hoveredVertex ->
                update (CreateEdge(selectedVertex, hoveredVertex)) { state with selected = SelectedNone } shared window

            | _ -> updateState { state with selected = state.hover }

        | MouseMove mousePoint ->
            printfn $"{mousePoint}"

            let vertexWithin =
                CreasePattern.pointWithin
                    (theme.pointerCloseDistance
                     / (max state.translation.xRatio state.translation.yRatio))
                    (Translation.pointToVertex state.translation mousePoint)
                    shared.frame.creasePattern

            match vertexWithin with
            | Some vertex ->
                updateState
                    { state with
                          hover = SelectedVertex vertex }
            | None -> updateState { state with hover = SelectedNone }


        | ToggleShowVertices _ ->
            updateState
                { state with
                      showVertices = not state.showVertices }


    (* Drawing *)

    let canvas (state: State) (creasePattern: CreasePattern) =
        let edgeLines =
            List.map (CreasePatternComponents.edgeLine state.translation) (CreasePattern.edges creasePattern)
            |> List.rev

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
                        Canvas.name canvasName ]

    let buttons dispatch =
        let iconButtons : IView list =
            [ Form.imageButton
                  {| icon = Icons.adjust
                     onClick = Msg.ToggleShowVertices >> dispatch |} ]

        StackPanel.create [ StackPanel.children iconButtons ]

    let view (state: State) (creasePattern: CreasePattern) dispatch =
        let creasePatternCanvas = canvas state creasePattern

        DockPanel.create
        <| [ DockPanel.background Theme.colors.background
             DockPanel.onPointerMoved (
                 (View.positionRelativeTo canvasName)
                 >> Msg.MouseMove
                 >> dispatch
             )

             Canvas.onPointerReleased (fun _ -> dispatch Msg.MouseClicked)
             DockPanel.children [ buttons dispatch
                                  creasePatternCanvas ] ]
