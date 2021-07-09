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

    type Msg =
        | MouseClicked
        | MouseMove of Point
        | CreaseEdge of Edge

    let canvasName = "Crease Pattern Canvas"

    let theme = {| pointerCloseDistance = 20. |}

    let update (msg: Msg) (state: State) : State =
        match msg with

        (* User Actions *)
        | MouseClicked ->
            match state.hover with
            | Some hover ->
                match state.selected with
                | SelectedNone ->
                    { state with
                          selected = SelectedOne hover }
                | SelectedOne selected ->
                    { state with
                          selected = SelectedTwo(selected, hover) }
                    
                // Todo: select the stuff
                | SelectedTwo _ -> { state with selected = SelectedNone }

            | None -> { state with selected = SelectedNone }


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
                | Some vertex, _ -> Some(VertexComponent vertex)
                | None, Some edge -> Some(EdgeComponent edge)
                | None, None -> None

            { state with
                  mousePosition = Some mousePoint
                  vertexPosition = Some convertedVertex
                  hover = hover }


        | CreaseEdge edge ->
            { state with
                  selected = SelectedNone
                  frame = Frame.mapCreasePattern (CreasePattern.addEdge edge) state.frame }


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

        let creasePatternComponent vertexView edgeView ``component`` =
            match ``component`` with
            | VertexComponent vertex -> vertexView state.translation vertex
            | EdgeComponent edge -> edgeView state.translation edge

        let hoverElement =
            Option.map
                (creasePatternComponent CreasePatternComponents.vertexHovered CreasePatternComponents.edgeLineHovered)
                state.hover

        let selectedElements =
            match state.selected with
            | SelectedNone -> []
            | SelectedOne sel -> [ sel ]
            | SelectedTwo (selOne, selTwo) -> [ selOne; selTwo ]

            |> List.map (
                creasePatternComponent CreasePatternComponents.vertexSelected CreasePatternComponents.edgeLineSelected
            )

        // This could be a slow point because of appending to the end of the list
        // If this gets too slow, cons to the beginning and reverse the list to maintain readability
        let canvasElements =
            []
            |> List.append edgeLines
            |> List.appendIf state.showVertices vertexPoints
            |> List.consWhenSome hoverElement
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
             DockPanel.onPointerReleased (Event.handleEvent Msg.MouseClicked >> dispatch)
             DockPanel.children [ canvas state ] ]
