namespace GuiLibrary

open Godot

type ShrinkMaximizeFs() as this =
    inherit TextureButton()

    override this._Pressed() =
        if OS.WindowMaximized then
            OS.WindowMaximized <- false
        else
            OS.WindowMaximized <- true
