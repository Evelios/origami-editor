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
    | DragLine of LineSegment2D
    | Edges of ComponentState * Edge list
    | Vertices of ComponentState * Point2D list
    | CreasePattern of CreasePattern
    | Size of float
    | Name of string

type CreasePatternDrawingState =
    { size: float
      creasePatternSize: Size
      graphElements: Set<CreasePatternComponent>
      dragLine: LineSegment2D option
      name: string option }

module CreasePatternDrawing =
    open Avalonia
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Controls.Shapes
    open Avalonia.Media

    open Utilities.Extensions

    (* Theming *)
    let theme =
        {| lineThickness = 2.
           vertexSize = 8.
           vertexHoveredSize = 10.
           vertexColor = Theme.colors.darkGray
           hoveredColor = Theme.colors.lightYellow
           pressedColor = Theme.colors.yellow
           selectedColor = Theme.colors.blue
           boundaryColor = Theme.colors.lighterGray
           mountainColor = Theme.colors.green
           valleyColor = Theme.colors.blue
           unassignedColor = Theme.colors.lighterGray
           flatColor = Theme.colors.lighterGray
           dragColor = Theme.colors.lighterGray |}

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
            | Preview -> Theme.colors.lightBlue
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
             Line.stroke (edgeColor options.state options.edge.Assignment)
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

    let private drawDragLine (translation: Translation) (line: LineSegment2D) : IView =
        let startScaled = line.Start * translation.xRatio
        let finishScaled = line.Finish * translation.xRatio

        Line.create
        <| [ Line.startPoint (Point(startScaled.X, startScaled.Y))
             Line.endPoint (Point(finishScaled.X, finishScaled.Y))
             Line.stroke theme.dragColor
             Line.strokeThickness theme.lineThickness
             Line.strokeLineCap PenLineCap.Round
             Line.strokeDashArray [ 5.; 2. ] ]
        :> IView

    let private draw (state: CreasePatternDrawingState) =
        let translation =
            Translation.create state.creasePatternSize state.size

        let graphElements =
            (Set.toSeq state.graphElements)
            |> Seq.sortBy
                (fun e ->
                    match e with
                    | EdgeElement _, Default -> 0
                    | VertexElement _, Default -> 1
                    | VertexElement _, Selected -> 4
                    | EdgeElement _, Selected -> 5
                    | EdgeElement _, Hovered -> 6
                    | VertexElement _, Hovered -> 7
                    | EdgeElement _, Pressed -> 8
                    | VertexElement _, Pressed -> 9)
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

        let maybeDragLineView =
            Option.map (drawDragLine translation) state.dragLine

        let canvasElements =
            List.ofSeq graphElements
            |> List.appendWhenSome maybeDragLineView

        let nameOption = Option.map Canvas.name state.name

        Canvas.create (
            [ Canvas.height state.size
              Canvas.width state.size
              Canvas.background Theme.palette.canvasBackground
              Canvas.children (List.ofSeq canvasElements) ]
            |> List.appendWhenSome nameOption
        )


    (**** API ****)

    (* Builders *)

    let private init =
        { size = 0.
          creasePatternSize = Size.empty
          graphElements = Set.empty
          dragLine = None
          name = None }

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
            (* Basic Attributes *)
            | Size size -> { state with size = size }
            | Name name -> { state with name = Some name }

            | GraphElements components -> addGraphElements components state
            | Edges (componentState, edges) -> addEdges componentState edges state
            | Vertices (componentState, vertices) -> addVertices componentState vertices state
            | DragLine line -> { state with dragLine = Some line }

            (* Crease Pattern Initialization *)
            | CreasePattern creasePattern ->
                { state with
                      creasePatternSize = CreasePattern.size creasePattern }
                |> addEdges Default (CreasePattern.edges creasePattern)
                |> addVertices Default (CreasePattern.vertices creasePattern)


        draw (List.fold attrToState init attrs)


    (* Attributes *)

    let name = Name

    let dragLine = DragLine

    let size = CreasePatternDrawingAttribute.Size

    /// Build the drawing from an initial crease pattern
    let creasePattern = CreasePattern

    let graphElements = GraphElements

    /// Add a list of components with a particular component state
    let graphElementsOf state elements =
        List.map (fun element -> (element, state)) elements
        |> GraphElements

    let graphElementOf state element =
        graphElementsOf state (List.singleton element)


    let edges state edges = Edges(state, edges)

    let vertices state vertices = Vertices(state, vertices)
