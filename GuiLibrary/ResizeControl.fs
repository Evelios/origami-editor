namespace GuiLibrary

open Godot


type ResizeNode = | Bottom

type DistanceToEdge = { bottom: Vector2 }

type WindowTracking =
    { mouseClickPosition: Vector2
      windowSize: Vector2
      resizeNode: ResizeNode }

type ResizeControlFs() =
    inherit Node()

    let mutable windowTrackingOption: WindowTracking option = None
    let mutable distanceToEdge = { bottom = Vector2() }

    member this.getResizeNode node =
        match node with
        | Bottom -> "Bottom"
        |> fun name -> this.GetNode<Control>(new NodePath(name))

    member this.nodeDistanceToEdge node =
        match node with
        | Bottom -> distanceToEdge.bottom

    override this._Ready() =
        distanceToEdge <-
            { bottom =
                  OS.WindowSize
                  - (this.getResizeNode Bottom).RectGlobalPosition }

    override this._Process _ =
        match windowTrackingOption with
        | None -> ()
        | Some windowTracking ->
            let newWindowSize =
                this.mousePosition ()
                + (this.nodeDistanceToEdge windowTracking.resizeNode)
                - windowTracking.mouseClickPosition

            OS.WindowSize <- Vector2(OS.WindowSize.x, newWindowSize.y)
//            this.getResizeNode(windowTracking.resizeNode).RectGlobalPosition



    member this.mousePosition() = this.GetTree().Root.GetMousePosition()
    member this.windowSize() = this.GetTree().Root.Size

    member this.handleMouseButton(event: InputEventMouseButton, node: ResizeNode) =
        if not OS.WindowMaximized && event.ButtonIndex = 1 then
            if event.Pressed then
                windowTrackingOption <-
                    Some
                        { mouseClickPosition = this.mousePosition ()
                          windowSize = this.windowSize ()
                          resizeNode = node }
            else
                windowTrackingOption <- None


    (* Signals *)
    member this._on_Bottom_gui_input(event: InputEvent) =
        match event with
        | :? InputEventMouseButton as mouseEvent -> this.handleMouseButton (mouseEvent, Bottom)
        | _ -> ()
