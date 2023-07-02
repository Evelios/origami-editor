namespace Gui.Tabs.CreasePatternTab.Drawing

open Math.Geometry
open Gui.Widgets
open Math.Units

module CreasePatternCanvas =
    open Avalonia
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Input

    open Gui
    open Origami
    open Utilities.Extensions

    type Msg =
        | MousePressed of Point
        | MouseReleased of Point * KeyModifiers
        | MouseMove of Point
        | CreaseEdge of Edge
        | KeyPressed of Key
        | RecalculateCreasePatternPreview

    let canvasName = "Crease Pattern Canvas"

    let theme =
        {| creasePatternSize = Length.cssPixels 500.
           pointerCloseDistance = Length.cssPixels 20. |}

    let rec update (msg: Msg) (state: CreasePatternTabState) : CreasePatternTabState =
        match msg with
        (* User Actions *)
        | KeyPressed key ->
            match key with
            | Key.Escape -> { state with selectedReferences = [] } |> update RecalculateCreasePatternPreview
            | _ -> state

        | MouseReleased(mousePosition, keyModifiers) ->
            let updatedState = update (MouseMove mousePosition) state

            if
                keyModifiers.HasFlag(KeyModifiers.Shift)
                && updatedState.pressed = updatedState.hover
            then
                match updatedState.pressed with
                | Some pressed ->
                    { updatedState with
                        pressed = None
                        selectedReferences = pressed :: updatedState.selectedReferences }
                    |> update RecalculateCreasePatternPreview
                | None -> updatedState |> update RecalculateCreasePatternPreview

            else
                match updatedState.pressed, updatedState.hover with
                | Some(VertexElement pressed), Some(VertexElement hover) when pressed <> hover ->

                    { updatedState with
                        pressed = None
                        creasePattern =
                            CreasePattern.addEdge
                                (Edge.betweenWithAssignment pressed hover EdgeAssignment.Unassigned)
                                updatedState.creasePattern }
                    |> update RecalculateCreasePatternPreview

                | Some _, _ -> { updatedState with pressed = None } |> update RecalculateCreasePatternPreview

                | _ -> updatedState |> update RecalculateCreasePatternPreview

        | MousePressed mousePosition ->
            let updatedState = update (MouseMove mousePosition) state

            { updatedState with
                pressed = updatedState.hover }

        | MouseMove mousePosition ->
            // The mouse position converted into crease pattern coordinates
            let convertedVertex = Translation.pointToVertex state.translation mousePosition

            // TODO: check these conversions
            let convertedCloseDistance =
                (theme.pointerCloseDistance
                 / (max state.translation.xRatio state.translation.yRatio))

            let vertexWithin =
                CreasePattern.pointWithin convertedCloseDistance convertedVertex state.creasePatternPreview

            let edgeWithin =
                CreasePattern.edgeWithin convertedCloseDistance convertedVertex state.creasePatternPreview

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
                creasePattern = CreasePattern.addEdge edge state.creasePattern
                selectedReferences = [] }
            |> update RecalculateCreasePatternPreview

        (* Internals *)
        | RecalculateCreasePatternPreview ->
            let axioms = Axiom.ofAxiomsFromElements state.axioms state.selectedReferences

            { state with
                creasePatternPreview = CreasePattern.performAxioms EdgeAssignment.Preview axioms state.creasePattern }




    (* Drawing *)

    type ViewState = { showVertices: bool }


    let canvas (state: CreasePatternTabState) =
        let pressedElement =
            Option.map (CreasePatternDrawing.graphElementOf ComponentState.Pressed) state.pressed

        let hoveredElement =
            Option.map (CreasePatternDrawing.graphElementOf ComponentState.Hovered) state.hover

        let dragLine =
            match state.pressed, state.hover, state.vertexPosition with
            | Some(VertexElement pressed), Some(VertexElement hover), _ ->
                CreasePatternDrawing.dragLine (LineSegment2D.from pressed hover) |> Some
            | Some(VertexElement pressed), _, Some vertexPosition ->
                CreasePatternDrawing.dragLine (LineSegment2D.from pressed vertexPosition)
                |> Some
            | _ -> None

        let userElements = [ pressedElement; hoveredElement; dragLine ] |> List.filterNone

        CreasePatternDrawing.create (
            [ CreasePatternDrawing.creasePattern state.creasePatternPreview
              CreasePatternDrawing.size theme.creasePatternSize
              CreasePatternDrawing.name canvasName
              CreasePatternDrawing.graphElementsOf ComponentState.Selected state.selectedReferences ]
            @ userElements
        )


    let view (state: CreasePatternTabState) dispatch =
        DockPanel.create
        <| [ DockPanel.background Theme.palette.canvasBackdrop
             DockPanel.onPointerMoved ((Event.positionRelativeTo canvasName) >> Msg.MouseMove >> dispatch)
             DockPanel.onPointerPressed ((Event.positionRelativeTo canvasName) >> Msg.MousePressed >> dispatch)
             DockPanel.onPointerReleased (fun e ->
                 Msg.MouseReleased((Event.positionRelativeTo canvasName e), e.KeyModifiers)
                 |> dispatch)
             DockPanel.focusable true
             DockPanel.onKeyUp (Event.handleKeyPress (KeyPressed >> dispatch))

             DockPanel.children [ canvas state ] ]
