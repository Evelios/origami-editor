namespace GuiLibrary.CreasePattern

open Godot

type Zoom = ZoomIn | ZoomOut

type ContainerFs() =
    inherit CenterContainer()

    override this._GuiInput event =
        match event with
        | :? InputEventMouseButton as mouseEvent -> this.handleMouseEvent mouseEvent
        | _ -> ()

    member this.handleMouseEvent (event: InputEventMouseButton) =
        match enum<ButtonList>(event.ButtonIndex) with
        | ButtonList.WheelUp -> ()
        | ButtonList.WheelDown -> ()
        | ButtonList.Left -> ()
        | ButtonList.Right -> ()
        | _ -> ()
