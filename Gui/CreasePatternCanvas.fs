namespace Gui

open Avalonia
open Avalonia.FuncUI.Types

module CreasePatternCanvas =

    open Avalonia.FuncUI.DSL
    open Avalonia.Controls
    open Avalonia.Controls.Shapes
    open CreasePattern
    open Fold

    type Msg = None

    let width = 500.
    let height = 500.
    
    let theme =
          {| lineThickness = 2.
             boundaryColor = Theme.colors.backgroundLight
             mountainColor = Theme.colors.tertiary
             valleyColor = Theme.colors.quaternary
             unassignedColor = Theme.colors.primary
             flatColor = Theme.colors.backgroundAccent
          |}


    (* Drawing *)

    let scaledEdges (creasePattern: CreasePattern) : Edge list =
        let xRatio =
            width / (creasePattern.bounds.maxX - creasePattern.bounds.minX)

        let yRatio =
            height / (creasePattern.bounds.maxY - creasePattern.bounds.minY)

        CreasePattern.edges creasePattern
        |> List.map (Edge.scale xRatio yRatio 1.)

    let canvas (creasePattern: CreasePattern) =
        let edgeColor (edgeType: EdgeAssignment): string =
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

        let edgeLines =
            List.map asLine (scaledEdges creasePattern)

        Canvas.create [ Canvas.height height
                        Canvas.width width
                        Canvas.background Theme.colors.foreground
                        Canvas.children edgeLines ]

    
    let view (creasePattern: CreasePattern) _ =
        DockPanel.create
        <| [ DockPanel.background Theme.colors.background
             DockPanel.children [ canvas creasePattern ] ]
