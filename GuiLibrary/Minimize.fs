namespace GuiLibrary

open Godot

type MinimizeFs() =
    inherit TextureButton()

    override this._Pressed() =
        OS.WindowMinimized <- true
