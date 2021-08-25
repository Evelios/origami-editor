namespace Gui.Tabs.ReferenceFinderTab

open Geometry

module ReferenceFinderTab =
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types
    open Avalonia.Layout
    open Elmish

    open CreasePattern
    open Gui
    open Gui.Widgets
    open Utilities.Extensions

    [<Literal>]
    let MaxSolutions = 4

    type Msg =
        | ChangeXInput of string
        | ChangeYInput of string
        | RunReferenceFinder
        | ReferenceFinderSolutions of ReferenceFinderSolution list

    let init : ReferenceFinderTabState =
        let x, y = 0.33, 0.33

        { x = x
          y = y
          xInput = x.ToString()
          yInput = y.ToString()
          referenceFinder = ReferenceFinder.init
          solutions = [] }

    let update (msg: Msg) (state: ReferenceFinderTabState) : ReferenceFinderTabState * Cmd<Msg> =
        match msg with
        | ChangeXInput xString ->
            match String.parseFloat xString with
            | Some newX -> { state with x = newX }, Cmd.none
            | None -> state, Cmd.none

        | ChangeYInput yString ->
            match String.parseFloat yString with
            | Some newY -> { state with y = newY }, Cmd.none
            | None -> state, Cmd.none

        | RunReferenceFinder ->
            state,
            Cmd.OfFunc.perform
                (fun () ->
                    ReferenceFinder.bestFoldSequencesTo MaxSolutions (Point2D.xy state.x state.y) state.referenceFinder)
                ()
                Msg.ReferenceFinderSolutions

        | ReferenceFinderSolutions solutions -> { state with solutions = solutions }, Cmd.none

    let private toolBar (state: ReferenceFinderTabState) dispatch =
        let coordinates : IView list =
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
            Button.create [ Button.onClick (fun _ -> dispatch RunReferenceFinder)
                            Button.content "Find fold sequences" ]

        StackPanel.create
        <| [ StackPanel.orientation Orientation.Horizontal
             StackPanel.background Theme.palette.panelBackground
             StackPanel.children (coordinates @ [ actionButton ]) ]

    let private foldSequences solution =
        let title = Text.h1 "Fold Sequences" []

        let pointString (p: Point2D) =
            $"({roundFloatTo 4 p.X}, {roundFloatTo 4 p.Y})"

        let lineString (l: Line2D) =
            $"[{pointString l.Start}; {pointString l.Finish}]"

        // TODO: Allow string wrapping for text boxes and fix the steps width
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
            Text.h2 $"Distance: {roundFloatTo 6 solution.Distance}" []

        let error = Text.h2 $"Error: {solution.Error}" []

        let steps : IView =
            solution.Steps
            |> Seq.map description
            |> List.ofSeq
            |> Text.numberedList

        StackPanel.create
        <| [ StackPanel.orientation Orientation.Vertical
             StackPanel.background Theme.palette.panelBackground
             StackPanel.children [ title
                                   pointSolution
                                   distance
                                   error
                                   steps ]
             StackPanel.width 200. ]

    let private previews referenceFinderSolutions =
        let panelHeight = 200.
        let creasePatternHeight = panelHeight * 0.6

        let creasePatterns =
            List.map (fun rf -> rf.Solution) referenceFinderSolutions

        let title =
            Text.h1 "All Solutions" [ TextBlock.horizontalAlignment HorizontalAlignment.Center ]

        let solutionPreview cp =
            CreasePatternDrawing.create
                {| size = creasePatternHeight
                   creasePattern = cp |}
            :> IView

        let creasePatternViews =
            StackPanel.create
            <| [ StackPanel.orientation Orientation.Horizontal
                 StackPanel.spacing Theme.spacing.medium
                 StackPanel.verticalAlignment VerticalAlignment.Center
                 StackPanel.horizontalAlignment HorizontalAlignment.Center
                 StackPanel.children (List.map solutionPreview creasePatterns) ]

        StackPanel.create
        <| [ StackPanel.orientation Orientation.Vertical
             StackPanel.background Theme.palette.panelBackground
             StackPanel.spacing Theme.spacing.medium
             StackPanel.height panelHeight
             StackPanel.verticalAlignment VerticalAlignment.Center
             StackPanel.children [ title
                                   creasePatternViews ] ]


    let private creasePattern cp =
        let creasePatternSize = 300.

        DockPanel.create
        <| [ DockPanel.background Theme.palette.canvasBackdrop
             DockPanel.children [ CreasePatternDrawing.create
                                      {| size = creasePatternSize
                                         creasePattern = cp |} ] ]

    let view (state: ReferenceFinderTabState) dispatch =

        match state.solutions with
        | best :: _ ->
            DockPanel.create
            <| [ DockPanel.children
                 <| [ View.withAttr (StackPanel.dock Dock.Top) (toolBar state dispatch)
                      View.withAttr (StackPanel.dock Dock.Right) (foldSequences best)
                      View.withAttr (StackPanel.dock Dock.Bottom) (previews state.solutions)
                      creasePattern best.Solution ] ]
        | _ ->
            DockPanel.create
            <| [ DockPanel.children
                 <| [ View.withAttr (StackPanel.dock Dock.Top) (toolBar state dispatch)
                      creasePattern CreasePattern.empty ] ]
