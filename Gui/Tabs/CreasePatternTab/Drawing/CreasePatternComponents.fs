namespace Gui.Tabs.CreasePatternTab.Drawing

module CreasePatternComponents =
    open Avalonia.Controls.Shapes
    open Avalonia
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types
    open Avalonia.Media

    open Gui
    open CreasePattern
    open Geometry

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


    (* User Help Components *)

    let dragLine (translation: Translation) (start: Point2D) (finish: Point2D) : IView =
        let startScaled = start * translation.xRatio
        let finishScaled = finish * translation.xRatio

        Line.create
        <| [ Line.startPoint (Point(startScaled.X, startScaled.Y))
             Line.endPoint (Point(finishScaled.X, finishScaled.Y))
             Line.stroke theme.dragColor
             Line.strokeThickness theme.lineThickness
             Line.strokeLineCap PenLineCap.Round
             Line.strokeDashArray [ 5.; 2. ] ]
        :> IView


    (* Edges *)

    let private edgeColor (edgeType: EdgeAssignment) : string =
        match edgeType with
        | EdgeAssignment.Boundary -> theme.boundaryColor
        | EdgeAssignment.Mountain -> theme.mountainColor
        | EdgeAssignment.Valley -> theme.valleyColor
        | EdgeAssignment.Unassigned -> theme.unassignedColor
        | EdgeAssignment.Flat -> theme.flatColor

    let edgeLine
        (options: {| translation: Translation
                     edge: Edge
                     color: string |})
        : IView =
        let scaledEdge =
            Edge.scale options.translation.yRatio options.edge

        Line.create
        <| [ Line.startPoint (Point(scaledEdge.Crease.Start.X, scaledEdge.Crease.Start.Y))
             Line.endPoint (Point(scaledEdge.Crease.Finish.X, scaledEdge.Crease.Finish.Y))
             Line.stroke options.color
             Line.strokeThickness theme.lineThickness
             Line.strokeLineCap PenLineCap.Round ]
        :> IView

    let edgeLineDefault translation edge =
        edgeLine
            {| translation = translation
               edge = edge
               color = edgeColor edge.assignment |}

    let edgeLineHovered translation edge =
        edgeLine
            {| translation = translation
               edge = edge
               color = theme.hoveredColor |}

    let edgeLinePressed translation edge =
        edgeLine
            {| translation = translation
               edge = edge
               color = theme.pressedColor |}

    let edgeLineSelected translation edge =
        edgeLine
            {| translation = translation
               edge = edge
               color = theme.selectedColor |}


    (* Vertices *)

    let vertexPoint
        (options: {| translation: Translation
                     color: string
                     size: float
                     vertex: Point2D |})
        : IView =
        let scaledVertex =
            Point2D.scale options.translation.xRatio options.translation.yRatio options.vertex

        Ellipse.create
        <| [ Ellipse.width options.size
             Ellipse.height options.size
             Ellipse.left (scaledVertex.X - options.size / 2.)
             Ellipse.top (scaledVertex.Y - options.size / 2.)
             Ellipse.fill options.color ]
        :> IView

    let vertexDefault (translation: Translation) (vertex: Point2D) : IView =
        vertexPoint
            {| translation = translation
               vertex = vertex
               color = theme.vertexColor
               size = theme.vertexSize |}


    let vertexHovered (translation: Translation) (vertex: Point2D) : IView =
        vertexPoint
            {| translation = translation
               vertex = vertex
               color = theme.hoveredColor
               size = theme.vertexHoveredSize |}

    let vertexPressed (translation: Translation) (vertex: Point2D) : IView =
        vertexPoint
            {| translation = translation
               vertex = vertex
               color = theme.pressedColor
               size = theme.vertexHoveredSize |}

    let vertexSelected (translation: Translation) (vertex: Point2D) : IView =
        vertexPoint
            {| translation = translation
               vertex = vertex
               color = theme.selectedColor
               size = theme.vertexSize |}
