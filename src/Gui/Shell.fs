namespace Gui

open Avalonia.FuncUI.Types

module Shell =
    open Avalonia.Controls
    open Avalonia.FuncUI.Components.Hosts
    open Avalonia.FuncUI.Elmish
    open Avalonia.FuncUI.DSL
    open Elmish


    open Gui.Tabs.CreasePatternTab
    open Gui.Tabs.ReferenceFinderTab

    let title = "Origami Editor"

    [<RequireQualifiedAccess>]
    type Tab =
        | CreasePattern
        | ReferenceFinder

    type State =
        { tab: Tab
          creasePattern: CreasePatternTabState
          referenceFinder: ReferenceFinderTabState }

    type Msg =
        | CreasePatternTabMsg of CreasePatternTab.Msg
        | ReferenceFinderTabMsg of ReferenceFinderTab.Msg

    let init : State =
        { tab = Tab.CreasePattern
          creasePattern = CreasePatternTab.init
          referenceFinder = ReferenceFinderTab.init }

    let update (msg: Msg) (state: State) (window: Window) : State * Cmd<Msg> =
        match msg with
        | CreasePatternTabMsg creasePatternTabMsg ->
            let creasePatternTabState, cmd =
                CreasePatternTab.update creasePatternTabMsg state.creasePattern window

            { state with
                  creasePattern = creasePatternTabState },
            Cmd.map CreasePatternTabMsg cmd

        | ReferenceFinderTabMsg referenceFinderTabMsg ->
            let referenceFinderTabState, cmd =
                ReferenceFinderTab.update referenceFinderTabMsg state.referenceFinder

            { state with
                  referenceFinder = referenceFinderTabState },
            Cmd.map ReferenceFinderTabMsg cmd

    let view (state: State) dispatch =
        let creasePattern =
            CreasePatternTab.view state.creasePattern (CreasePatternTabMsg >> dispatch)

        let referenceFinder =
            ReferenceFinderTab.view state.referenceFinder (ReferenceFinderTabMsg >> dispatch)

        let tabs : IView list =
            [ TabItem.create [ TabItem.header "Crease Pattern"
                               TabItem.content creasePattern ]
              TabItem.create [ TabItem.header "Reference Finder"
                               TabItem.content referenceFinder ] ]


        TabControl.create [ TabControl.tabStripPlacement Dock.Top // Change this property to tell the app where to show the tab bar
                            TabControl.viewItems tabs ]


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
