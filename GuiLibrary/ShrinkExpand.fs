namespace GuiLibrary

open Godot

type ShrinkMaximizeFs() as this =
    inherit TextureButton()

    let icon =
        {| maximize = GD.Load("res://Assets/Icons/maximize.svg") :?> Texture
           shrink = GD.Load("res://Assets/Icons/shrink.svg") :?> Texture |}

    let setTexture() =
        this.TextureNormal <-
            match OS.WindowMaximized with
            | true -> icon.shrink
            | false -> icon.maximize

    override this._Ready() =
        setTexture()

    override this._Pressed() =
        if OS.WindowMaximized then
            OS.WindowMaximized <- false
        else
            OS.WindowMaximized <- true
        setTexture()
