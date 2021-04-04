namespace GuiLibrary

open Godot


type ResizeNode =
    | Top
    | Bottom
    | Left
    | Right

type DistanceToEdge =
    { top: Vector2
      bottom: Vector2
      left: Vector2
      right: Vector2 }

type WindowTracking =
    { mouseClickPosition: Vector2
      windowPosition: Vector2
      windowSize: Vector2
      resizeNode: ResizeNode }

type ResizeControlFs() =
    inherit Node()

    let mutable windowTrackingOption: WindowTracking option = None

    let mutable distanceToEdge =
        { top = Vector2()
          bottom = Vector2()
          left = Vector2()
          right = Vector2() }

    let minSize = Vector2(600.f, 400.f)

    member this.getResizeNode node =
        match node with
        | Top -> "Top"
        | Bottom -> "Bottom"
        | Left -> "Left"
        | Right -> "Right"
        |> fun name -> this.GetNode<Control>(new NodePath(name))

    member this.nodeDistanceToEdge node =
        match node with
        | Top -> distanceToEdge.top
        | Bottom -> distanceToEdge.bottom
        | Left -> distanceToEdge.left
        | Right -> distanceToEdge.right

    override this._Ready() =
        distanceToEdge <-
            { top = Vector2()
              bottom =
                  OS.WindowSize
                  - (this.getResizeNode Bottom).RectGlobalPosition
              left = Vector2()
              right =
                  OS.WindowSize
                  - (this.getResizeNode Right).RectGlobalPosition

            }

    override this._Process _ =
        match windowTrackingOption with
        | None -> ()
        | Some windowTracking ->
            if Input.IsMouseButtonPressed(1) then
                this.updateWindowSize windowTracking
                this.resetResizeBarPositions ()
            else
                windowTrackingOption <- None

    member this.updateWindowSize(windowTracking: WindowTracking) =
        let windowOffset =
            (this.mousePosition ())
            - windowTracking.mouseClickPosition
            + (this.nodeDistanceToEdge windowTracking.resizeNode)

        match windowTracking.resizeNode with
        | Top ->
            OS.WindowPosition <- OS.WindowPosition + Vector2(0.f, windowOffset.y)
            OS.WindowSize <- Vector2(windowTracking.windowSize.x, OS.WindowSize.y - windowOffset.y |> max minSize.y)
        | Bottom ->
            OS.WindowSize <-
                Vector2
                    (windowTracking.windowSize.x,
                     windowTracking.windowSize.y + windowOffset.y
                     |> max minSize.y)
        | Left ->
            OS.WindowPosition <- OS.WindowPosition + Vector2(windowOffset.x, 0.f)
            OS.WindowSize <- Vector2(OS.WindowSize.x - windowOffset.x |> max minSize.x, windowTracking.windowSize.y)
        | Right ->
            OS.WindowSize <-
                Vector2
                    (windowTracking.windowSize.x + windowOffset.x
                     |> max minSize.x,
                     windowTracking.windowSize.y)

    member this.resetResizeBarPositions() =
        let bottom = this.getResizeNode Bottom
        bottom.RectPosition <- Vector2(0.f, OS.WindowSize.y - distanceToEdge.bottom.y)

    member this.mousePosition() = this.GetTree().Root.GetMousePosition()
    member this.windowSize() = this.GetTree().Root.Size
    member this.windowPosition() = OS.WindowPosition

    member this.handleInputEvent (event: InputEvent) (node: ResizeNode) =
        match event with
        | :? InputEventMouseButton as mouseEvent -> this.handleMouseButton (mouseEvent, node)
        | _ -> ()

    member this.handleMouseButton(event: InputEventMouseButton, node: ResizeNode) =
        // Clear the window tracking in case of an errored state
        if Option.isSome windowTrackingOption
           && not <| Input.IsMouseButtonPressed(1) then
            GD.Print("Clear Window Tracking")
            windowTrackingOption <- None

        elif not OS.WindowMaximized && event.ButtonIndex = 1 then
            if event.Pressed then
                GD.Print("Pressed")

                windowTrackingOption <-
                    Some
                        { mouseClickPosition = this.mousePosition ()
                          windowSize = this.windowSize ()
                          windowPosition = this.windowPosition ()
                          resizeNode = node }

                GD.Print(windowTrackingOption)

            else
                GD.Print("Released")
                windowTrackingOption <- None




    (* Signals *)

    member this._on_Top_gui_input(event: InputEvent) = this.handleInputEvent event Top

    member this._on_Bottom_gui_input(event: InputEvent) = this.handleInputEvent event Bottom
    member this._on_Left_gui_input(event: InputEvent) = this.handleInputEvent event Left
    member this._on_Right_gui_input(event: InputEvent) = this.handleInputEvent event Right
