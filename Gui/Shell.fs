namespace Gui

module Shell =
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

    type Msg =
        (* Component Messages *)
        | FileMenuMsg of FileMenu.Msg
        | FileSettingsMsg of FileSettings.Msg
        | IconBarMsg of IconBar.Msg
        | InfoBarMsg of InfoBar.Msg
        | CreasePatternCanvasMsg of CreasePatternCanvas.Msg

        (* Global Messages *)
        | UpdateTitle

    let title = "Origami Editor"

    let init =
        let frame = Frame.create

        let translation =
            Translation.create frame.creasePattern Theme.creasePattern.maxLength

        { filePath = None
          frame = frame
          showVertices = true
          hover = None
          selected = None
          selectedReferences = []
          mousePosition = None
          vertexPosition = None
          translation = translation
          pageSize = translation.pageSize }

    let update (msg: Msg) (state: State) (window: Window) : State * Cmd<Msg> =
        match msg with

        (* Component Messages *)
        | FileMenuMsg fileMenuMsg ->
            let newState, fileMenuCommand, external =
                FileMenu.update fileMenuMsg state window

            let cmd = Cmd.map FileMenuMsg fileMenuCommand

            match external with
            | FileMenu.External.FoldFileLoaded -> newState, Cmd.ofMsg UpdateTitle
            | FileMenu.External.DoNothing -> newState, (Cmd.batch [ cmd; Cmd.ofMsg UpdateTitle ])

        | InfoBarMsg infoBarMsg -> InfoBar.update infoBarMsg state, Cmd.none

        | FileSettingsMsg fileSettingsMsg ->
            { state with
                  frame = FileSettings.update fileSettingsMsg state.frame },
            Cmd.none

        | IconBarMsg iconBarMsg -> IconBar.update iconBarMsg state, Cmd.none

        | CreasePatternCanvasMsg creasePatternCanvasMsg ->
            CreasePatternCanvas.update creasePatternCanvasMsg state, Cmd.none

        (* Global Messages*)
        | UpdateTitle ->
            window.Title <-
                match state.filePath with
                | Some path -> $"{title} - {Path.GetFileNameWithoutExtension(path)}"
                | None -> title

            state, Cmd.none

    let view (state: State) dispatch =
        DockPanel.create
        <| [ DockPanel.background Theme.palette.panelBackground
             DockPanel.children [ DockPanel.child Top (FileMenu.view (FileMenuMsg >> dispatch))
                                  DockPanel.child Bottom (InfoBar.view state (InfoBarMsg >> dispatch))
                                  DockPanel.child Left (IconBar.view state (IconBarMsg >> dispatch))
                                  DockPanel.child Right (FileSettings.view state.frame (FileSettingsMsg >> dispatch))
                                  CreasePatternCanvas.view state (CreasePatternCanvasMsg >> dispatch) ] ]

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

            Program.mkProgram (fun () -> init, Cmd.none) updateWithServices view
            |> Program.withHost this
            |> Program.run
