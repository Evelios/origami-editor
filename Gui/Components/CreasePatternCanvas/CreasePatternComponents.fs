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

    let theme =
        {| lineThickness = 2.
           vertexSize = 8.
           vertexHoveredSize = 10.
           vertexColor = Theme.colors.backgroundDark
           vertexHoveredColor = Theme.colors.primary
           vertexSelectedColor = Theme.colors.tertiary
           boundaryColor = Theme.colors.backgroundLight
           mountainColor = Theme.colors.tertiary
           valleyColor = Theme.colors.quaternary
           unassignedColor = Theme.colors.primary
           flatColor = Theme.colors.backgroundAccent |}


    (* Edges *)

    let edgeColor (edgeType: EdgeAssignment) : string =
        match edgeType with
        | EdgeAssignment.Boundary -> theme.boundaryColor
        | EdgeAssignment.Mountain -> theme.mountainColor
        | EdgeAssignment.Valley -> theme.valleyColor
        | EdgeAssignment.Unassigned -> theme.unassignedColor
        | EdgeAssignment.Flat -> theme.flatColor

    let edgeLine (translation: Translation) (edge: Edge) : IView =
        let scaledEdge =
            Edge.scale translation.xRatio translation.yRatio 1. edge

        Line.create
        <| [ Line.startPoint (Point(Vertex.x scaledEdge.start, Vertex.y scaledEdge.start))
             Line.endPoint (Point(Vertex.x scaledEdge.finish, Vertex.y scaledEdge.finish))
             Line.stroke (edgeColor edge.assignment)
             Line.strokeThickness theme.lineThickness
             Line.strokeLineCap PenLineCap.Round ]
        :> IView


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
               color = theme.vertexHoveredColor
               size = theme.vertexHoveredSize |}
               
    let vertexSelected (translation: Translation) (vertex: Vertex) : IView =
        vertexPoint
            {| translation = translation
               vertex = vertex
               color = theme.vertexSelectedColor
               size = theme.vertexSize |}
