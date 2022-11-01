namespace Gui.Tabs.ReferenceFinderTab

open Avalonia.Media
open Math.Units
open Math.Geometry

module ReferenceFinderTab =
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types
    open Avalonia.Layout
    open Elmish

    open Origami
    open Gui
    open Gui.Widgets
    open Utilities.Extensions

    [<Literal>]
    let MaxSolutions = 4

    type Msg =
        // User Inputs
        | ChangeXInput of string
        | ChangeYInput of string
        // User Execution
        | RunReferenceFinder
        | ReferenceFinderSolutions of ReferenceFinderSolution<UserSpace> array
        // Steps
        | HoverStep of int
        | LeaveStep
        // Solution Previews
        | HoverSolutionPreview of int
        | LeaveSolutionPreview
        | SelectSolutionPreview of int

    let init: ReferenceFinderTabState =
        let x, y =
            Length.meters 0.33, Length.meters 0.33

        { x = x
          y = y
          xInput = x.ToString()
          yInput = y.ToString()
          referenceFinder = ReferenceFinder.init
          solutions = [||]
          hoveredStep = None
          activeSolution = 0
          hoveredSolution = None }

    let update (msg: Msg) (state: ReferenceFinderTabState) : ReferenceFinderTabState * Cmd<Msg> =
        match msg with
        // User Input

        | ChangeXInput xString ->
            match String.parseFloat xString with
            | Some newX -> { state with x = Length.meters newX }, Cmd.none
            | None -> state, Cmd.none

        | ChangeYInput yString ->
            match String.parseFloat yString with
            | Some newY -> { state with y = Length.meters newY }, Cmd.none
            | None -> state, Cmd.none

        // User Execution

        | RunReferenceFinder ->
            state,
            Cmd.OfFunc.perform
                (fun () ->
                    ReferenceFinder.bestFoldSequencesTo MaxSolutions (Point2D.xy state.x state.y) state.referenceFinder)
                ()
                Msg.ReferenceFinderSolutions

        | ReferenceFinderSolutions solutions -> { state with solutions = solutions }, Cmd.none

        // Steps
        | HoverStep index -> { state with hoveredStep = Some index }, Cmd.none

        | LeaveStep -> { state with hoveredStep = None }, Cmd.none

        // Solutions

        | HoverSolutionPreview index -> { state with hoveredSolution = Some index }, Cmd.none

        | LeaveSolutionPreview -> { state with hoveredSolution = None }, Cmd.none

        | SelectSolutionPreview index -> { state with activeSolution = index }, Cmd.none

    let private toolBar (state: ReferenceFinderTabState) dispatch =
        let coordinates: IView list =
            [ Form.textItem
                  {| name = "X"
                     value = state.xInput
                     onSelected = ChangeXInput >> dispatch
                     labelPlacement = Orientation.Horizontal |}
              Form.textItem
                  {| name = "Y"
                     value = state.yInput
                     onSelected = ChangeYInput >> dispatch
                     labelPlacement = Orientation.Horizontal |} ]

        let actionButton =
            Button.create [
                Button.onClick (fun _ -> dispatch RunReferenceFinder)
                Button.content "Find fold sequences"
            ]

        StackPanel.create
        <| [ StackPanel.orientation Orientation.Horizontal
             StackPanel.background Theme.palette.panelBackground
             StackPanel.children (coordinates @ [ actionButton ]) ]

    let private foldSequences maybeHoverIndex solution dispatch =
        let title = Text.h1 "Fold Sequences" []

        let pointString (p: Point2D<Meters, UserSpace>) =
            let x =
                p.X |> Length.inMeters |> Float.roundFloatTo 4

            let y =
                p.Y |> Length.inMeters |> Float.roundFloatTo 4

            $"({x}, {y})"

        let lineString (l: Line2D<Meters, UserSpace>) =
            $"[{pointString l.Start}; {pointString l.Finish}]"

        let description axiomAction =
            match axiomAction with
            | AxiomAction.One (p1, p2) ->
                "Axiom One: Crease a line through the two points "
                + $"{pointString p1} & {pointString p2}"
            | AxiomAction.Two (p1, p2) ->
                "Axiom Two: Overlap points and create a line in between the two points "
                + $"{pointString p1} & {pointString p2}"
            | AxiomAction.Three (l1, l2) ->
                "Axiom Three: Line up the two creases and fold to create the angle bisector between "
                + $"{lineString l1} & {lineString l2}"

        let pointSolution =
            Text.h2 $"Solution: {pointString solution.Point}" []

        let distance =
            let distance =
                solution.Distance
                |> Length.inMeters
                |> Float.roundFloatTo 6

            Text.h2 $"Distance: {distance}" []

        let error =
            Text.h2 $"Error: {solution.Error}" []


        /// Create a widget of numbered items. Items can be hovered over to change styling
        /// Eg.
        ///     1. First
        ///     2. Second
        ///     3. Third
        let hoverableNumberedList maybeHoverIndex items =
            let numberedItem index text =
                TextBlock.create (
                    [ TextBlock.text $"{index + 1}. {text}"
                      TextBlock.margin Theme.spacing.small
                      TextBlock.textWrapping TextWrapping.Wrap
                      TextBlock.onPointerEnter (fun _ -> HoverStep index |> dispatch)
                      TextBlock.onPointerLeave (fun _ -> dispatch LeaveStep) ]
                    |> List.appendIf
                        (Option.contains index maybeHoverIndex)
                        [ TextBlock.foreground Theme.palette.primary ]
                )
                :> IView

            StackPanel.create
            <| [ StackPanel.orientation Orientation.Vertical
                 StackPanel.children (List.mapi numberedItem items)
                 StackPanel.margin Theme.spacing.medium ]
            :> IView

        let steps: IView =
            solution.Steps
            |> Seq.map description
            |> List.ofSeq
            |> hoverableNumberedList maybeHoverIndex

        StackPanel.create
        <| [ StackPanel.orientation Orientation.Vertical
             StackPanel.background Theme.palette.panelBackground
             StackPanel.children [
                 title
                 pointSolution
                 distance
                 error
                 steps
             ]
             StackPanel.width 200. ]

    let private previews selected referenceFinderSolutions dispatch =
        let panelHeight = 200.
        let creasePatternHeight = panelHeight * 0.5

        let title =
            Text.h1 "All Solutions" [ TextBlock.horizontalAlignment HorizontalAlignment.Center ]

        let solutionPreview index referenceFinderSolution =
            let creasePatternDrawing =
                CreasePatternDrawing.create
                <| [ CreasePatternDrawing.size creasePatternHeight
                     CreasePatternDrawing.creasePattern referenceFinderSolution.Solution ]

            let error =
                Text.paragraph
                    $"e = {referenceFinderSolution.Error}"
                    [ TextBlock.horizontalAlignment HorizontalAlignment.Center ]

            StackPanel.create [
                StackPanel.orientation Orientation.Vertical
                StackPanel.spacing Theme.spacing.small
                StackPanel.margin Theme.spacing.medium
                StackPanel.children [
                    creasePatternDrawing
                    error
                ]
            ]
            |> Accents.selectable
                (Option.contains index selected)
                [ Border.onPointerEnter (fun _ -> (HoverSolutionPreview index |> dispatch))
                  Border.onPointerLeave (fun _ -> (dispatch LeaveSolutionPreview))
                  Border.onPointerReleased (fun _ -> (SelectSolutionPreview index |> dispatch)) ]
            :> IView

        let solutionPreviews =
            Array.mapi solutionPreview referenceFinderSolutions
            |> List.ofArray

        let creasePatternViews =
            StackPanel.create
            <| [ StackPanel.orientation Orientation.Horizontal
                 StackPanel.spacing Theme.spacing.small
                 StackPanel.verticalAlignment VerticalAlignment.Center
                 StackPanel.horizontalAlignment HorizontalAlignment.Center
                 StackPanel.children solutionPreviews ]

        StackPanel.create
        <| [ StackPanel.orientation Orientation.Vertical
             StackPanel.background Theme.palette.panelBackground
             StackPanel.spacing Theme.spacing.medium
             StackPanel.height panelHeight
             StackPanel.verticalAlignment VerticalAlignment.Center
             StackPanel.children [
                 title
                 creasePatternViews
             ] ]


    let private creasePattern cp hoveredStep =
        let creasePatternSize = 300.

        let stepElements =
            match hoveredStep with
            | Some axiomAction ->
                let results =
                    CreasePattern.axiomResult EdgeAssignment.Flat axiomAction cp
                    |> Seq.map (fun e -> (EdgeElement e, ComponentState.Selected))
                    |> List.ofSeq

                let references =
                    match axiomAction with
                    | AxiomAction.One (p1, p2) -> [ VertexElement p1; VertexElement p2 ]
                    | AxiomAction.Two (p1, p2) -> [ VertexElement p1; VertexElement p2 ]
                    | AxiomAction.Three (l1, l2) ->
                        [ l1; l2 ]
                        |> List.filterMap (fun e -> CreasePattern.boundedEdge e cp)
                        |> List.map EdgeElement

                    |> List.map (fun e -> (e, ComponentState.Pressed))

                List.append results references

            | None -> []

        DockPanel.create
        <| [ DockPanel.background Theme.palette.canvasBackdrop
             DockPanel.children
             <| [ CreasePatternDrawing.create
                  <| [ CreasePatternDrawing.creasePattern cp
                       CreasePatternDrawing.size creasePatternSize
                       CreasePatternDrawing.graphElements stepElements ] ] ]

    let view (state: ReferenceFinderTabState) dispatch =
        match Array.tryItem state.activeSolution state.solutions with
        | Some selected ->
            let hoveredStep =
                Option.bind (fun i -> Array.tryItem i (Array.ofSeq selected.Steps)) state.hoveredStep

            let creasePatternStep =
                Option.bind (fun i -> Array.tryItem i (Array.ofSeq selected.CreasePatterns)) state.hoveredStep
                |> Option.defaultValue selected.Solution

            DockPanel.create
            <| [ DockPanel.children
                 <| [ DockPanel.child Direction.Top (toolBar state dispatch)
                      DockPanel.child Direction.Right (foldSequences state.hoveredStep selected dispatch)
                      DockPanel.child Direction.Bottom (previews state.hoveredSolution state.solutions dispatch)
                      creasePattern creasePatternStep hoveredStep ] ]
        | None ->
            DockPanel.create
            <| [ DockPanel.children
                 <| [ View.withAttr (StackPanel.dock Dock.Top) (toolBar state dispatch)
                      creasePattern CreasePattern.empty None ] ]
