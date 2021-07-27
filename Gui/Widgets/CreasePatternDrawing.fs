namespace Gui.Widgets

open Avalonia.FuncUI.Types
open Geometry

module CreasePatternDrawing =

    open Avalonia
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Controls.Shapes
    open Avalonia.Media

    open Gui
    open CreasePattern


    (* Types *)

    type private ComponentState =
        | Default
        | Selected
        | Hovered

    type private CanvasComponent = Component * ComponentState


    (* Theming *)

    let private theme =
        {| lineThickness = 2.
           vertexSize = 8.
           vertexHoveredSize = 10. |}

    let private componentColor componentType state : string =
        let stateColor state =
            match state with
            | Default -> Theme.colors.darkGray
            | Hovered -> Theme.colors.blue
            | Selected -> Theme.colors.yellow

        match componentType with
        | VertexComponent _ -> stateColor state
        | EdgeComponent edge ->
            match state with
            | Default ->
                match edge.assignment with
                | Boundary -> Theme.colors.lighterGray
                | Mountain -> Theme.colors.green
                | Valley -> Theme.colors.blue
                | Unassigned -> Theme.colors.lighterGray
            | _ -> stateColor state


    (* Basic Components *)

    let private edgeLine
        (options: {| translation: Translation
                     edge: Edge
                     state: ComponentState |})
        : IView =
        let scaledEdge =
            Edge.scale options.translation.yRatio options.edge

        Line.create
        <| [ Line.startPoint
             <| Point(scaledEdge.crease.start.x, scaledEdge.crease.start.y)
             Line.endPoint
             <| Point(scaledEdge.crease.finish.x, scaledEdge.crease.finish.y)
             Line.stroke (componentColor (EdgeComponent options.edge) options.state)
             Line.strokeThickness theme.lineThickness
             Line.strokeLineCap PenLineCap.Round ]
        :> IView

    let private vertexPoint
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
             Ellipse.left (scaledVertex.x - options.size / 2.)
             Ellipse.top (scaledVertex.y - options.size / 2.)
             Ellipse.fill (componentColor (VertexComponent options.vertex) options.state) ]
        :> IView

    let private drawComponent
        (options: {| translation: Translation
                     element: Component
                     state: ComponentState |})
        : IView =
        match options.element with
        | VertexComponent vertex ->
            vertexPoint
                {| translation = options.translation
                   vertex = vertex
                   size = 4.
                   state = options.state |}
        | EdgeComponent edge ->
            edgeLine
                {| translation = options.translation
                   edge = edge
                   state = options.state |}


    let private draw creasePattern =
        let size = 500.
        let translation = Translation.create creasePattern size

        let edges =
            CreasePattern.edges creasePattern
            |> List.map EdgeComponent

        let vertices =
            CreasePattern.vertices creasePattern
            |> List.map VertexComponent

        let components =
            edges @ vertices
            |> List.map
                (fun element ->
                    drawComponent
                        {| translation = translation
                           element = element
                           state = Default |})

        Canvas.create [ Canvas.height size
                        Canvas.width size
                        Canvas.background Theme.palette.canvasBackground
                        Canvas.children components
                        // TODO: fix name
                        Canvas.name "TODO: Fix Me" ]


    (* API *)

    let create creasePattern = draw creasePattern
