namespace Gui.Tabs.CreasePatternTab


module CreasePatternTab =
    open System.IO

    open Elmish
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL

    open CreasePattern
    open Gui
    open Gui.Widgets
    open Gui.Tabs.CreasePatternTab
    open Gui.Tabs.CreasePatternTab.Drawing

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

    // TODO: preview next creases
    let init : CreasePatternTabState =
        let creasePattern = CreasePattern.create

        let translation =
            Translation.create creasePattern Theme.creasePattern.maxLength

        { creasePattern = creasePattern
          filePath = None
          pressed = None
          hover = None
          selectedReferences = []
          mousePosition = None
          vertexPosition = None
          axioms =
              [ Axiom.One
                Axiom.Two
                Axiom.Three ]
          showVertices = true
          translation = translation
          pageSize = translation.pageSize }

    let update (msg: Msg) (state: CreasePatternTabState) (window: Window) : CreasePatternTabState * Cmd<Msg> =
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
                  creasePattern = FileSettings.update fileSettingsMsg state.creasePattern },
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

    let view (state: CreasePatternTabState) dispatch =
        DockPanel.create
        <| [ DockPanel.background Theme.palette.panelBackground
             DockPanel.children
             <| [ DockPanel.child Top (FileMenu.view (FileMenuMsg >> dispatch))
                  DockPanel.child Bottom (InfoBar.view state (InfoBarMsg >> dispatch))
                  DockPanel.child Left (IconBar.view state (IconBarMsg >> dispatch))
                  DockPanel.child Right (FileSettings.view state.creasePattern (FileSettingsMsg >> dispatch))
                  CreasePatternCanvas.view state (CreasePatternCanvasMsg >> dispatch) ] ]
