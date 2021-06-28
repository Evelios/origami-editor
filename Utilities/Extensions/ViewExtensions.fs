namespace Utilities.Extensions


module View =
    open System
    open Avalonia.Controls
    open Avalonia.LogicalTree

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
