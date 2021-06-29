namespace Gui

module Shell =
    open System
    open System.IO

    open Elmish
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Components.Hosts
    open Avalonia.FuncUI.Elmish

    open Fold
    open CreasePattern
    open Gui.Components
    open Gui.Widgets
    open Gui.Components.CreasePatternCanvas

    type State =
        { frame: Frame
          showVertices: bool
          creasePatternCanvas: CreasePatternCanvas.State }

    type Msg =
        (* Component Messages *)
        | FileMenuMsg of FileMenu.Msg
        | FileSettingsMsg of FileSettings.Msg
        | IconBarMsg of IconBar.Msg
        | CreasePatternCanvasMsg of CreasePatternCanvas.Msg

        (* Global Messages *)
        | UpdateTitle of string
        | LoadFoldFile of string
        | SaveFoldFileToPath of string

    let title = "Origami Editor"

    let init =
        let frame = Frame.create

        { frame = frame
          showVertices = true
          creasePatternCanvas = CreasePatternCanvas.init frame.creasePattern },
        Cmd.none

    let update (msg: Msg) (state: State) (window: Window) : State * Cmd<_> =
        match msg with

        (* Component Messages *)
        | FileSettingsMsg fileSettingsMsg ->
            { state with
                  frame = FileSettings.update fileSettingsMsg state.frame },
            Cmd.none

        | FileMenuMsg fileMenuMsg ->
            let fileMenuCommand, external = FileMenu.update fileMenuMsg window
            let cmd = Cmd.map FileMenuMsg fileMenuCommand

            match external with
            | FileMenu.CreateNewFile -> { state with frame = Frame.empty }, cmd
            | FileMenu.SelectedFoldFilePath paths ->
                match Array.tryHead paths with
                | Some path -> state, Cmd.ofMsg (LoadFoldFile path)
                | None -> state, cmd
            | FileMenu.LoadFoldFile path ->
                state,
                Cmd.batch [ Cmd.ofMsg <| LoadFoldFile path
                            cmd ]
            | FileMenu.SaveFoldFileToPath path ->
                state,
                Cmd.batch [ Cmd.ofMsg <| SaveFoldFileToPath path
                            cmd ]
            | FileMenu.DoNothing -> state, cmd

        | IconBarMsg iconBarMsg ->
            let external = IconBar.update iconBarMsg

            match external with
            | IconBar.External.ToggleShowVertices ->
                { state with
                      showVertices = not state.showVertices },
                Cmd.none

        | CreasePatternCanvasMsg creasePatternCanvasMsg ->
            let creasePatternCanvasState, external =
                CreasePatternCanvas.update creasePatternCanvasMsg state.creasePatternCanvas state.frame.creasePattern

            let newState =
                { state with
                      creasePatternCanvas = creasePatternCanvasState }

            match external with
            | CreasePatternCanvas.External.CreateEdge edge ->
                { newState with
                      frame = Frame.mapCreasePattern (CreasePattern.addEdge edge) state.frame },
                Cmd.none
            | CreasePatternCanvas.External.DoNothing -> newState, Cmd.none


        (* Global Messages*)
        | UpdateTitle newTitle ->
            window.Title <- $"{title} - {newTitle}"
            state, Cmd.none

        | LoadFoldFile foldPath ->
            match FileLoader.loadFoldFile foldPath with
            | Ok foldContents ->
                { state with
                      frame = Frame.fromFoldFrame foldContents.keyFrame },
                Cmd.ofMsg
                <| UpdateTitle(Path.GetFileNameWithoutExtension foldPath)

            | Error error ->
                printfn $"An error occured loading fold file: {foldPath}{Environment.NewLine}{error}"
                state, Cmd.none

        | SaveFoldFileToPath foldPath ->
            let foldText =
                Fold.empty
                |> Fold.setKeyframe (Frame.toFoldFrame state.frame)
                |> FoldJson.toJson

            File.WriteAllText(foldPath, foldText)

            state, Cmd.none


    let view (state: State) dispatch =
        let creasePatternCanvas =
            CreasePatternCanvas.view
                state.creasePatternCanvas
                { showVertices = state.showVertices }
                state.frame.creasePattern
                (CreasePatternCanvasMsg >> dispatch)

        DockPanel.create
        <| [ DockPanel.background Theme.palette.panelBackground
             DockPanel.children [ DockPanel.child Top (FileMenu.view (FileMenuMsg >> dispatch))
                                  DockPanel.child
                                      Left
                                      (IconBar.view { showVertices = state.showVertices } (IconBarMsg >> dispatch))
                                  DockPanel.child Right (FileSettings.view state.frame (FileSettingsMsg >> dispatch))
                                  creasePatternCanvas ] ]

    type MainWindow() as this =
        inherit HostWindow()

        do
            base.Title <- title
            base.Height <- Theme.window.height
            base.Width <- Theme.window.width
            base.MinHeight <- Theme.window.height
            base.MinWidth <- Theme.window.width
            this.HasSystemDecorations <- true

            let updateWithServices (msg: Msg) (state: State) = update msg state this

            Program.mkProgram (fun () -> init) updateWithServices view
            |> Program.withHost this
            |> Program.run
