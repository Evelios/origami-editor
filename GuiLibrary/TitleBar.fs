namespace GuiLibrary

open Godot

type TitleBarFs() =
    inherit PanelContainer()

    let mutable startPosition: Vector2 = Vector2()
    let mutable following = false

    override this._GuiInput event =
        match event with
        | :? InputEventMouseButton as mouseEvent ->
            if mouseEvent.ButtonIndex = 1 && not OS.WindowMaximized then
                following <- not following
                startPosition <- mouseEvent.Position

        | _ -> ()

    override this._Process _ =
        if following then
            OS.WindowPosition <-
                OS.WindowPosition
                + this.GetViewport().GetMousePosition()
                - startPosition
