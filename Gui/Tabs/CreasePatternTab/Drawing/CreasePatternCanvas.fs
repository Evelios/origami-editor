namespace Gui.Tabs.CreasePatternTab.Drawing

open Utilities
open Utilities.Collections

module CreasePatternCanvas =

    open Avalonia
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Input

    open Gui
    open CreasePattern
    open Utilities.Extensions

    type Msg =
        | MousePressed of Point
        | MouseReleased of Point * KeyModifiers
        | MouseMove of Point
        | CreaseEdge of Edge

    let canvasName = "Crease Pattern Canvas"

    let theme = {| pointerCloseDistance = 20. |}

    let rec update (msg: Msg) (state: CreasePatternTabState) : CreasePatternTabState =
        match msg with
        (* User Actions *)
        | MouseReleased (mousePosition, keyModifiers) ->
            let updatedState = update (MouseMove mousePosition) state

            if keyModifiers.HasFlag(KeyModifiers.Shift)
               && updatedState.pressed = updatedState.hover then
                match updatedState.pressed with
                | Some pressed ->
                    { updatedState with
                          pressed = None
                          selectedReferences = pressed :: updatedState.selectedReferences }
                | None -> updatedState

            else
                match updatedState.pressed, updatedState.hover with
                | Some (VertexElement pressed), Some (VertexElement hover) when pressed <> hover ->

                    { updatedState with
                          pressed = None
                          creasePattern =
                              CreasePattern.addEdge
                                  (Edge.betweenWithAssignment pressed hover Unassigned)
                                  updatedState.creasePattern }

                | Some _, _ -> { updatedState with pressed = None }

                | _ -> updatedState

        | MousePressed mousePosition ->
            let updatedState = update (MouseMove mousePosition) state

            { updatedState with
                  pressed = updatedState.hover }

        | MouseMove mousePosition ->
            // The mouse position converted into crease pattern coordinates
            let convertedVertex =
                Translation.pointToVertex state.translation mousePosition

            let convertedCloseDistance =
                (theme.pointerCloseDistance
                 / (max state.translation.xRatio state.translation.yRatio))

            let vertexWithin =
                CreasePattern.pointWithin convertedCloseDistance convertedVertex state.creasePattern

            let edgeWithin =
                CreasePattern.edgeWithin convertedCloseDistance convertedVertex state.creasePattern
                
            let hover =
                match vertexWithin, edgeWithin with
                | Some vertex, _ -> Some(VertexElement vertex)
                | None, Some edge -> Some(EdgeElement edge)
                | None, None -> None

            { state with
                  mousePosition = Some mousePosition
                  vertexPosition = Some convertedVertex
                  hover = hover }

        | CreaseEdge edge ->
            { state with
                  creasePattern = CreasePattern.addEdge edge state.creasePattern }


    (* Drawing *)

    type ViewState = { showVertices: bool }


    let canvas (state: CreasePatternTabState) =
        let edgeLines =
            Seq.map
                (CreasePatternComponents.edgeLineDefault state.translation)
                (CreasePattern.edges state.creasePattern)
            |> Seq.rev

        let vertexPoints =
            Seq.map
                (CreasePatternComponents.vertexDefault state.translation)
                (CreasePattern.vertices state.creasePattern)

        let creasePatternComponent vertexView edgeView ``component`` =
            match ``component`` with
            | VertexElement vertex -> vertexView state.translation vertex
            | EdgeElement edge -> edgeView state.translation edge

        let hoverElement =
            Option.map
                (creasePatternComponent CreasePatternComponents.vertexHovered CreasePatternComponents.edgeLineHovered)
                state.hover

        let pressedElement =
            Option.map
                (creasePatternComponent CreasePatternComponents.vertexPressed CreasePatternComponents.edgeLinePressed)
                state.pressed

        let dragLine =
            match state.pressed, state.hover, state.vertexPosition with
            | Some (VertexElement pressed), Some (VertexElement hover), _ ->
                Some(CreasePatternComponents.dragLine state.translation pressed hover)
            | Some (VertexElement pressed), _, Some vertexPosition ->
                Some(CreasePatternComponents.dragLine state.translation pressed vertexPosition)
            | _ -> None

        let selectedElements =
            List.map
                (creasePatternComponent CreasePatternComponents.vertexSelected CreasePatternComponents.edgeLineSelected)
                state.selectedReferences

        let canvasElements =
            []
            |> List.append (List.ofSeq edgeLines)
            |> List.appendIf state.showVertices (List.ofSeq vertexPoints)
            |> List.consWhenSome dragLine
            |> List.consWhenSome hoverElement
            |> List.consWhenSome pressedElement
            |> List.append selectedElements
            |> List.rev

        Canvas.create [ Canvas.height state.pageSize.Height
                        Canvas.width state.pageSize.Width
                        Canvas.background Theme.palette.canvasBackground
                        Canvas.children canvasElements
                        Canvas.name canvasName ]


    let view (state: CreasePatternTabState) dispatch =
        DockPanel.create
        <| [ DockPanel.background Theme.palette.canvasBackdrop
             DockPanel.onPointerMoved (
                 (Event.positionRelativeTo canvasName)
                 >> Msg.MouseMove
                 >> dispatch
             )
             DockPanel.onPointerPressed (
                 (Event.positionRelativeTo canvasName)
                 >> Msg.MousePressed
                 >> dispatch
             )
             DockPanel.onPointerReleased
                 (fun e ->
                     Msg.MouseReleased((Event.positionRelativeTo canvasName e), e.KeyModifiers)
                     |> dispatch)

             DockPanel.children [ canvas state ] ]
