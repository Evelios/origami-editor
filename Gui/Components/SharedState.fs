namespace Gui

open CreasePattern

type SharedState = { frame: Frame }


module SharedState =
    let init = { frame = Frame.create }
