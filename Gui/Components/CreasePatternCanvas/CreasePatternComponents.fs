namespace Gui.Components.CreasePatternCanvas



module CreasePatternComponents =
    open Avalonia.Controls.Shapes
    open Avalonia
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types
    open Avalonia.Media

    open Fold
    open Gui
    open CreasePattern
    open Geometry

    let theme =
        {| lineThickness = 2.
           vertexSize = 8.
           vertexHoveredSize = 10.
           vertexColor = Theme.colors.darkGray
           hoveredColor = Theme.colors.yellow
           selectedColor = Theme.colors.blue
           boundaryColor = Theme.colors.lighterGray
           mountainColor = Theme.colors.green
           valleyColor = Theme.colors.blue
           unassignedColor = Theme.colors.lighterGray
           flatColor = Theme.colors.lighterGray |}


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
            Edge.scale options.translation.xRatio options.translation.yRatio 1. options.edge

        Line.create
        <| [ Line.startPoint (Point(Vertex.x scaledEdge.start, Vertex.y scaledEdge.start))
             Line.endPoint (Point(Vertex.x scaledEdge.finish, Vertex.y scaledEdge.finish))
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
                     vertex: Vertex |})
        : IView =
        let scaledVertex =
            Vertex.scale options.translation.xRatio options.translation.yRatio 1. options.vertex

        Ellipse.create
        <| [ Ellipse.width options.size
             Ellipse.height options.size
             Ellipse.left ((Vertex.x scaledVertex) - options.size / 2.)
             Ellipse.top ((Vertex.y scaledVertex) - options.size / 2.)
             Ellipse.fill options.color ]
        :> IView

    let vertexDefault (translation: Translation) (vertex: Vertex) : IView =
        vertexPoint
            {| translation = translation
               vertex = vertex
               color = theme.vertexColor
               size = theme.vertexSize |}


    let vertexHovered (translation: Translation) (vertex: Vertex) : IView =
        vertexPoint
            {| translation = translation
               vertex = vertex
               color = theme.hoveredColor
               size = theme.vertexHoveredSize |}

    let vertexSelected (translation: Translation) (vertex: Vertex) : IView =
        vertexPoint
            {| translation = translation
               vertex = vertex
               color = theme.selectedColor
               size = theme.vertexSize |}
