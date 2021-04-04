namespace GuiLibrary

open Godot


type ResizeNode =
    | Top
    | Bottom
    | Left
    | Right
    | TopLeft
    | TopRight
    | BottomRight
    | BottomLeft

type WindowTracking =
    { mouseClickPosition: Vector2
      windowPosition: Vector2
      windowSize: Vector2
      resizeNode: ResizeNode }

type ResizeControlFs() =
    inherit Node()

    let mutable windowTrackingOption: WindowTracking option = None

    let minSize = Vector2(600.f, 400.f)

    member this.getResizeNode node =
        match node with
        | Top -> "Top"
        | Bottom -> "Bottom"
        | Left -> "Left"
        | Right -> "Right"
        | TopLeft -> "Top left"
        | TopRight -> "Top Right"
        | BottomRight -> "Bottom Right"
        | BottomLeft -> "Bottom Left"
        |> fun name -> this.GetNode<Control>(new NodePath(name))

    override this._Process _ =
        match windowTrackingOption with
        | None -> ()
        | Some windowTracking ->
            if Input.IsMouseButtonPressed(1) then
                this.updateWindowSize windowTracking
            //                this.resetResizeBarPositions ()
            else
                windowTrackingOption <- None

    member this.updateWindowSize(windowTracking: WindowTracking) =
        let windowOffset =
            (this.mousePosition ())
            - windowTracking.mouseClickPosition

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
        | TopLeft ->
            OS.WindowPosition <- OS.WindowPosition + windowOffset

            OS.WindowSize <-
                Vector2
                    (OS.WindowSize.x - windowOffset.x |> max minSize.x,
                     OS.WindowSize.y - windowOffset.y |> max minSize.y)
        | TopRight ->
            OS.WindowPosition <- OS.WindowPosition + Vector2(0.f, windowOffset.y)

            OS.WindowSize <-
                Vector2
                    (windowTracking.windowSize.x + windowOffset.x
                     |> max minSize.x,
                     OS.WindowSize.y - windowOffset.y |> max minSize.y)
        | BottomRight ->
            OS.WindowSize <-
                Vector2
                    (windowTracking.windowSize.x + windowOffset.x
                     |> max minSize.x,
                     windowTracking.windowSize.y + windowOffset.y
                     |> max minSize.y)

        | BottomLeft ->
            OS.WindowPosition <- OS.WindowPosition + Vector2(windowOffset.x, 0.f)

            OS.WindowSize <-
                Vector2
                    (OS.WindowSize.x - windowOffset.x |> max minSize.x,
                     windowTracking.windowSize.y + windowOffset.y
                     |> max minSize.y)

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
            windowTrackingOption <- None

        elif not OS.WindowMaximized && event.ButtonIndex = 1 then
            if event.Pressed then
                windowTrackingOption <-
                    Some
                        { mouseClickPosition = this.mousePosition ()
                          windowSize = this.windowSize ()
                          windowPosition = this.windowPosition ()
                          resizeNode = node }

            else
                windowTrackingOption <- None

        else
            ()


    (* Signals *)

    member this._on_Top_gui_input(event: InputEvent) = this.handleInputEvent event Top

    member this._on_Bottom_gui_input(event: InputEvent) = this.handleInputEvent event Bottom
    member this._on_Left_gui_input(event: InputEvent) = this.handleInputEvent event Left
    member this._on_Right_gui_input(event: InputEvent) = this.handleInputEvent event Right
    member this._on_Top_Left_gui_input(event: InputEvent) = this.handleInputEvent event TopLeft
    member this._on_Top_Right_gui_input(event: InputEvent) = this.handleInputEvent event TopRight
    member this._on_Bottom_Right_gui_input(event: InputEvent) = this.handleInputEvent event BottomRight
    member this._on_Bottom_Left_gui_input(event: InputEvent) = this.handleInputEvent event BottomLeft
