namespace GuiLibrary

open Godot

type ShrinkExpandFs() =
    inherit TextureButton()

    override this._Pressed() =
        OS.WindowMaximized <- not OS.WindowMaximized
