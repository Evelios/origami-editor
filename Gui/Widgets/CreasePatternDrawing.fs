namespace Gui.Widgets

open Avalonia.FuncUI.Types
open Geometry

module CreasePatternDrawing =
    // Todo: Extracting drawing api

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

    type private CanvasComponent = GraphElement * ComponentState


    (* Theming *)

    let private theme =
        {| lineThickness = 2.
           vertexSize = 8.
           vertexHoveredSize = 10. |}

    let private stateColor state =
        match state with
        | Default -> Theme.colors.darkGray
        | Hovered -> Theme.colors.blue
        | Selected -> Theme.colors.yellow

    let private vertexColor state =
        match state with
        | Default -> Theme.colors.darkGray
        | Hovered -> Theme.colors.blue
        | Selected -> Theme.colors.yellow


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
             <| Point(scaledEdge.crease.start.x, scaledEdge.crease.start.y)
             Line.endPoint
             <| Point(scaledEdge.crease.finish.x, scaledEdge.crease.finish.y)
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
             Ellipse.left (scaledVertex.x - options.size / 2.)
             Ellipse.top (scaledVertex.y - options.size / 2.)
             Ellipse.fill (vertexColor options.state) ]
        :> IView

    let private draw creasePattern =
        let size = 500.
        let translation = Translation.create creasePattern size

        let edges =
            CreasePattern.edges creasePattern
            |> List.map
                (fun edge ->
                    drawEdge
                        {| translation = translation
                           edge = edge
                           state = ComponentState.Default |})

        let vertices =
            CreasePattern.vertices creasePattern
            |> List.map
                (fun vertex ->
                    drawVertex
                        {| translation = translation
                           vertex = vertex
                           size = 4.
                           state = ComponentState.Default |})

        let components = edges @ vertices

        Canvas.create [ Canvas.height size
                        Canvas.width size
                        Canvas.background Theme.palette.canvasBackground
                        Canvas.children components
                        // TODO: fix name
                        Canvas.name "TODO: Fix Me" ]


    (* API *)

    let create creasePattern = draw creasePattern
