namespace Utilities.Extensions


module View =
    open System
    
    open Avalonia
    open Avalonia.Controls
    open Avalonia.LogicalTree
    open Avalonia.Input

    /// Try to find a child control of a given name using breadth first search
    let findChildControl (name: string) (source: IControl) : IControl option =
        let rec findChildControlHelper (children: ILogical list) =
            match children with
            | first :: remaining ->
                if (first :?> IControl).Name = name then
                    Some(first :?> IControl)
                else
                    findChildControlHelper (remaining @ (List.ofSeq first.LogicalChildren))

            | [] -> None

        findChildControlHelper (List.ofSeq source.LogicalChildren)

    /// Traverse to the root of the tree and do a breadth first search for the element
    let findControl (name: String) (source: IControl) : IControl option =
        if source.Name = name then
            Some source
        else
            findChildControl name (source.VisualRoot :?> IControl)
            
    /// Given a pointer event, get the position relative to a particular control name
    /// Useful for triggering off of mouse movement events
    let positionRelativeTo (name: String) (event: PointerEventArgs) =
         let maybeVisual =
             findControl name (event.Source :?> IControl)

         match maybeVisual with
         | Some visual -> event.GetPosition(visual)
         | None -> Point(infinity, infinity)