namespace Gui

module CreasePatternCanvas =

    open Avalonia.FuncUI.DSL
    open CreasePattern

    type State = { creasePattern: CreasePattern }

    type Msg = None

    let init = { creasePattern = CreasePattern.create }


    let update (msg: Msg) (state: State) : State =
        match msg with
        | _ -> state

    let view (state: State) dispatch = Canvas.create []
