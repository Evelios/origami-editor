namespace Utilities.Extensions



module Event =
    open System
    open Avalonia
    open Avalonia.Controls
    open Avalonia.Input
    open Avalonia.Interactivity

    let handleEvent msg (event: RoutedEventArgs) =
        event.Handled <- true
        msg

    /// Given a pointer event, get the position relative to a particular control name
    /// Useful for triggering off of mouse movement events
    let positionRelativeTo (name: String) (event: PointerEventArgs) =
        let maybeVisual =
            View.findControl name (event.Source :?> IControl)

        match maybeVisual with
        | Some visual -> event.GetPosition(visual)
        | None -> Point(infinity, infinity)