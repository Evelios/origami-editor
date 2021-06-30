namespace Gui.Components.CreasePatternCanvas

module CreasePatternCanvas =

    open Avalonia
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL

    open CreasePattern
    open Fold
    open Gui
    open Utilities.Collections
    open Utilities.Extensions

    [<RequireQualifiedAccess>]
    type External =
        | MouseMoved
        | DoNothing

    type Msg =
        | MouseMove of Point
        | MouseClicked

    let canvasName = "Crease Pattern Canvas"

    let theme = {| pointerCloseDistance = 20. |}

    let update (msg: Msg) (state: State) : State * External =

        match msg with
        (* User Actions *)
        | MouseClicked ->
            match (state.hover, state.selected) with
            | SelectedVertex hoveredVertex, SelectedVertex selectedVertex ->
                if hoveredVertex = selectedVertex then
                    { state with selected = SelectedNone }, External.DoNothing

                else
                    let edge =
                        Edge.create
                            { start = hoveredVertex
                              finish = selectedVertex
                              assignment = EdgeAssignment.Unassigned }

                    { state with
                          selected = SelectedNone
                          frame = Frame.mapCreasePattern (CreasePattern.addEdge edge) state.frame },
                    External.DoNothing

            | _ -> { state with selected = state.hover }, External.DoNothing

        | MouseMove mousePoint ->
            // The mouse position converted into crease pattern coordinates
            let convertedVertex =
                Translation.pointToVertex state.translation mousePoint

            let convertedCloseDistance =
                (theme.pointerCloseDistance
                 / (max state.translation.xRatio state.translation.yRatio))

            let vertexWithin =
                CreasePattern.pointWithin convertedCloseDistance convertedVertex state.frame.creasePattern

            let edgeWithin =
                CreasePattern.edgeWithin convertedCloseDistance convertedVertex state.frame.creasePattern

            let hover =
                match vertexWithin, edgeWithin with
                | Some vertex, _ -> SelectedVertex vertex
                | None, Some edge -> SelectedEdge edge
                | None, None -> SelectedNone

            { state with
                  mousePosition = Some mousePoint
                  vertexPosition = Some convertedVertex
                  hover = hover },
            External.MouseMoved


    (* Drawing *)

    type ViewState = { showVertices: bool }


    let canvas (state: State) =
        let edgeLines =
            List.map
                (CreasePatternComponents.edgeLineDefault state.translation)
                (CreasePattern.edges state.frame.creasePattern)
            |> List.rev

        let vertexPoints =
            List.map
                (CreasePatternComponents.vertexDefault state.translation)
                (CreasePattern.vertices state.frame.creasePattern)

        let hoverElement =
            match state.hover with
            | SelectedVertex vertex ->
                CreasePatternComponents.vertexHovered state.translation vertex
                |> Some
            | SelectedEdge edge ->
                CreasePatternComponents.edgeLineHovered state.translation edge
                |> Some
            | SelectedNone -> None

        let selectedElement =
            match state.selected with
            | SelectedVertex vertex ->
                CreasePatternComponents.vertexSelected state.translation vertex
                |> Some
            | SelectedEdge edge ->
                CreasePatternComponents.edgeLineSelected state.translation edge
                |> Some
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
                        Canvas.background Theme.palette.canvasBackground
                        Canvas.children canvasElements
                        Canvas.name canvasName ]


    let view (state: State) dispatch =
        DockPanel.create
        <| [ DockPanel.background Theme.palette.canvasBackdrop
             DockPanel.onPointerMoved (
                 (Event.positionRelativeTo canvasName)
                 >> Msg.MouseMove
                 >> dispatch
             )
             DockPanel.onPointerReleased (Event.handleEvent Msg.MouseClicked >> dispatch)
             DockPanel.children [ canvas state ] ]
