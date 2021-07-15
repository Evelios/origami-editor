namespace Gui.Components.CreasePatternCanvas

module CreasePatternCanvas =

    open Avalonia
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL

    open CreasePattern
    open Gui
    open Utilities.Collections
    open Utilities.Extensions

    type Msg =
        | MousePressed of Point
        | MouseReleased of Point
        | MouseMove of Point
        | CreaseEdge of Edge

    let canvasName = "Crease Pattern Canvas"

    let theme = {| pointerCloseDistance = 20. |}

    let rec update (msg: Msg) (state: State) : State =
        match msg with
        (* User Actions *)
        | MouseReleased mousePosition ->
            let updatedState = update (MouseMove mousePosition) state

            match updatedState.pressed, updatedState.hover with
            | Some (VertexComponent pressed), Some (VertexComponent hover) when pressed <> hover ->

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

            match updatedState.hover with
            | Some (VertexComponent _) ->
                { updatedState with
                      pressed = updatedState.hover }
            | _ -> updatedState



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
                | Some vertex, _ -> Some(VertexComponent vertex)
                | None, Some edge -> Some(EdgeComponent edge)
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


    let canvas (state: State) =
        let edgeLines =
            List.map
                (CreasePatternComponents.edgeLineDefault state.translation)
                (CreasePattern.edges state.creasePattern)
            |> List.rev

        let vertexPoints =
            List.map
                (CreasePatternComponents.vertexDefault state.translation)
                (CreasePattern.vertices state.creasePattern)

        let creasePatternComponent vertexView edgeView ``component`` =
            match ``component`` with
            | VertexComponent vertex -> vertexView state.translation vertex
            | EdgeComponent edge -> edgeView state.translation edge

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
            | Some (VertexComponent pressed), Some (VertexComponent hover), _ ->
                Some(CreasePatternComponents.dragLine state.translation pressed hover)
            | Some (VertexComponent pressed), _, Some vertexPosition ->
                Some(CreasePatternComponents.dragLine state.translation pressed vertexPosition)
            | _ -> None

        let selectedElements =
            match state.selected with
            | Some sel -> [ sel ]
            | None -> []

            |> List.map (
                creasePatternComponent CreasePatternComponents.vertexSelected CreasePatternComponents.edgeLineSelected
            )

        let canvasElements =
            []
            |> List.append edgeLines
            |> List.appendIf state.showVertices vertexPoints
            |> List.consWhenSome dragLine
            |> List.consWhenSome hoverElement
            |> List.consWhenSome pressedElement
            |> List.append selectedElements
            |> List.rev

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
             DockPanel.onPointerPressed (
                 (Event.positionRelativeTo canvasName)
                 >> Msg.MousePressed
                 >> dispatch
             )
             DockPanel.onPointerReleased (
                 (Event.positionRelativeTo canvasName)
                 >> Msg.MouseReleased
                 >> dispatch
             )
             DockPanel.children [ canvas state ] ]
