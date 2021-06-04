namespace Gui

open Avalonia
open Avalonia.FuncUI.Types

module CreasePatternCanvas =

    open Avalonia.FuncUI.DSL
    open Avalonia.Controls
    open Avalonia.Controls.Shapes

    open CreasePattern
    open Fold
    open Utilities.Collections

    type Msg = None

    let maxLength = 500.

    let theme =
        {| lineThickness = 2.
           boundaryColor = Theme.colors.backgroundLight
           mountainColor = Theme.colors.tertiary
           valleyColor = Theme.colors.quaternary
           unassignedColor = Theme.colors.primary
           flatColor = Theme.colors.backgroundAccent |}


    (* Drawing *)


    let canvas (creasePattern: CreasePattern) =
        let creasePatternSize = CreasePattern.size creasePattern

        let pageSize =
            Size.withMaxSize maxLength creasePatternSize

        let scaledEdges : Edge list =
            let xRatio = pageSize.width / creasePatternSize.width

            let yRatio =
                pageSize.height / creasePatternSize.height

            CreasePattern.edges creasePattern
            |> List.map (Edge.scale xRatio yRatio 1.)

        let edgeColor (edgeType: EdgeAssignment) : string =
            match edgeType with
            | EdgeAssignment.Boundary -> theme.boundaryColor
            | EdgeAssignment.Mountain -> theme.mountainColor
            | EdgeAssignment.Valley -> theme.valleyColor
            | EdgeAssignment.Unassigned -> theme.unassignedColor
            | EdgeAssignment.Flat -> theme.flatColor

        let asLine edge =
            Line.create
            <| [ Line.startPoint (Point(Vertex.x edge.start, Vertex.y edge.start))
                 Line.endPoint (Point(Vertex.x edge.finish, Vertex.y edge.finish))
                 Line.stroke (edgeColor edge.assignment)
                 Line.strokeThickness theme.lineThickness
                 Line.strokeLineCap Media.PenLineCap.Round ]
            :> IView

        let edgeLines = List.map asLine scaledEdges

        Canvas.create [ Canvas.height pageSize.height
                        Canvas.width pageSize.width
                        Canvas.background Theme.colors.foreground
                        Canvas.children edgeLines ]


    let view (creasePattern: CreasePattern) _ =
        DockPanel.create
        <| [ DockPanel.background Theme.colors.background
             DockPanel.children [ canvas creasePattern ] ]
