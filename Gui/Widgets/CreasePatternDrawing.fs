namespace Gui.Widgets

open Avalonia.FuncUI.Types
open CreasePattern
open Geometry
open Gui

/// Possible states that a crease pattern component can be in
type ComponentState =
    | Default
    | Hovered
    | Selected
    | Pressed

type CreasePatternComponent = GraphElement * ComponentState

type CreasePatternDrawingAttribute =
    | GraphElements of CreasePatternComponent list
    | Edges of ComponentState * Edge list
    | Vertices of ComponentState * Point2D list
    | CreasePattern of CreasePattern
    | Size of float

type CreasePatternDrawingState =
    { size: float
      creasePatternSize: Size
      graphElements: Set<CreasePatternComponent> }

module CreasePatternDrawing =
    open Avalonia
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Controls.Shapes
    open Avalonia.Media



    (* Theming *)

    let private theme =
        {| lineThickness = 2.
           vertexSize = 8.
           vertexHoveredSize = 10. |}

    let private stateColor state =
        match state with
        | Default -> Theme.colors.darkGray
        | Selected -> Theme.colors.blue
        | Hovered -> Theme.colors.lightYellow
        | Pressed -> Theme.colors.yellow

    let private vertexColor = stateColor

    let private edgeColor state assignment =
        match state with
        | Default ->
            match assignment with
            | Boundary -> Theme.colors.lighterGray
            | Mountain -> Theme.colors.green
            | Valley -> Theme.colors.blue
            | Flat -> Theme.colors.lighterGray
            | Unassigned -> Theme.colors.lighterGray
        | _ -> stateColor state


    (* Basic Components *)

    let private drawEdge
        (options: {| translation: Translation
                     edge: Edge
                     state: ComponentState |})
        : IView =
        let scaledEdge =
            Edge.scale options.translation.yRatio options.edge

        Line.create
        <| [ Line.startPoint
             <| Point(scaledEdge.Crease.Start.X, scaledEdge.Crease.Start.Y)
             Line.endPoint
             <| Point(scaledEdge.Crease.Finish.X, scaledEdge.Crease.Finish.Y)
             Line.stroke (edgeColor options.state options.edge.assignment)
             Line.strokeThickness theme.lineThickness
             Line.strokeLineCap PenLineCap.Round ]
        :> IView

    let private drawVertex
        (options: {| translation: Translation
                     size: float
                     vertex: Point2D
                     state: ComponentState |})
        : IView =
        let scaledVertex =
            Point2D.scale options.translation.xRatio options.translation.yRatio options.vertex

        Ellipse.create
        <| [ Ellipse.width options.size
             Ellipse.height options.size
             Ellipse.left (scaledVertex.X - options.size / 2.)
             Ellipse.top (scaledVertex.Y - options.size / 2.)
             Ellipse.fill (vertexColor options.state) ]
        :> IView

    let private draw (state: CreasePatternDrawingState) =
        let translation =
            Translation.create state.creasePatternSize state.size

        let components =
            (Set.toSeq state.graphElements)
            |> Seq.sortBy
                (fun e ->
                    match e with
                    | EdgeElement _, Default -> 0
                    | VertexElement _, Default -> 1
                    | VertexElement _, Selected -> 2
                    | EdgeElement _, Selected -> 3
                    | EdgeElement _, Hovered -> 4
                    | VertexElement _, Hovered -> 5
                    | EdgeElement _, Pressed -> 6
                    | VertexElement _, Pressed -> 7)
            |> Seq.map
                (fun e ->
                    match e with
                    | VertexElement vertex, vertexState ->
                        drawVertex
                            {| translation = translation
                               vertex = vertex
                               size = 4.
                               state = vertexState |}
                    | EdgeElement edge, edgeState ->
                        drawEdge
                            {| translation = translation
                               edge = edge
                               state = edgeState |})


        Canvas.create [ Canvas.height state.size
                        Canvas.width state.size
                        Canvas.background Theme.palette.canvasBackground
                        Canvas.children (List.ofSeq components)
                        // TODO: fix name
                        Canvas.name "TODO: Fix Me" ]


    (**** API ****)

    (* Builders *)

    let private init =
        { size = 0.
          creasePatternSize = Size.empty
          graphElements = Set.empty }

    /// Create the crease pattern drawing object
    let create attrs =
        let addGraphElements elements state =
            { state with
                  graphElements = Set.union state.graphElements (Set.ofSeq elements) }

        let addEdges componentState edges state =
            addGraphElements (Seq.map (fun edge -> EdgeElement edge, componentState) edges) state

        let addVertices componentState vertices state =
            addGraphElements (Seq.map (fun vertex -> VertexElement vertex, componentState) vertices) state

        let rec attrToState state attr =
            match attr with
            | GraphElements components -> addGraphElements components state
            | Edges (componentState, edges) -> addEdges componentState edges state
            | Vertices (componentState, vertices) -> addVertices componentState vertices state

            | CreasePattern creasePattern ->
                { state with
                      creasePatternSize = CreasePattern.size creasePattern }
                |> addEdges Default (CreasePattern.edges creasePattern)
                |> addVertices Default (CreasePattern.vertices creasePattern)
            | Size size -> { state with size = size }

        draw (List.fold attrToState init attrs)


    (* Attributes *)

    let size = CreasePatternDrawingAttribute.Size

    let graphElements = GraphElements

    let creasePattern = CreasePattern

    let edges state edges = Edges(state, edges)

    let vertices state vertices = Vertices(state, vertices)
