namespace GuiLibrary

open Godot

type CloseFs() =
    inherit TextureButton()

    override this._Pressed() =
        this.GetTree().Quit()
