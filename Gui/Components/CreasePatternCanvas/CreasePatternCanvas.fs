namespace Gui.Components.CreasePatternCanvas

open Avalonia.FuncUI.Types

module CreasePatternCanvas =

    open Avalonia
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL

    open CreasePattern
    open Fold
    open Gui
    open Utilities.Collections
    open Utilities.Extensions

    type Selectable =
        | SelectedVertex of Vertex
        | SelectedNone

    type State =
        { hover: Selectable
          selected: Selectable
          translation: Translation
          pageSize: Size }

    type External =
        | CreateEdge of Edge
        | DoNothing

    type Msg =
        | MouseMove of Point
        | MouseClicked

    let canvasName = "Crease Pattern Canvas"

    let theme =
        {| maxLength = 500.
           pointerCloseDistance = 20. |}

    let init (creasePattern: CreasePattern) =
        let translation =
            Translation.create creasePattern theme.maxLength

        { hover = SelectedNone
          selected = SelectedNone
          translation = translation
          pageSize = translation.pageSize }

    let rec update (msg: Msg) (state: State) (creasePattern: CreasePattern) : State * External =

        match msg with
        (* User Actions *)
        | MouseClicked ->
            match (state.hover, state.selected) with
            | SelectedVertex hoveredVertex, SelectedVertex selectedVertex ->
                if hoveredVertex = selectedVertex then
                    { state with selected = SelectedNone }, DoNothing

                else
                    let edge =
                        Edge.create
                            { start = hoveredVertex
                              finish = selectedVertex
                              assignment = EdgeAssignment.Unassigned }

                    { state with selected = SelectedNone }, CreateEdge edge

            | _ -> { state with selected = state.hover }, DoNothing

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
                      hover = SelectedVertex vertex },
                DoNothing
            | None -> { state with hover = SelectedNone }, DoNothing




    (* Drawing *)

    type ViewState = { showVertices: bool }


    let canvas (state: State) (viewState: ViewState) (creasePattern: CreasePattern) =
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
            |> List.concatIf viewState.showVertices vertexPoints
            |> List.appendWhenSome hoverElement
            |> List.appendWhenSome selectedElement

        Canvas.create [ Canvas.height state.pageSize.height
                        Canvas.width state.pageSize.width
                        Canvas.background Theme.palette.canvasBackground
                        Canvas.children canvasElements
                        Canvas.name canvasName ]


    let view (state: State) (viewState: ViewState) (creasePattern: CreasePattern) dispatch =
        let creasePatternCanvas = canvas state viewState creasePattern


        DockPanel.create
        <| [ DockPanel.background Theme.palette.canvasBackdrop
             DockPanel.onPointerMoved (
                 (Event.positionRelativeTo canvasName)
                 >> Msg.MouseMove
                 >> dispatch
             )
             DockPanel.onPointerReleased (Event.handleEvent Msg.MouseClicked >> dispatch)
             DockPanel.children [ creasePatternCanvas ] ]
