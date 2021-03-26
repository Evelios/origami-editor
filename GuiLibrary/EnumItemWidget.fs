namespace GuiLibrary

open Godot
open Utilities

type EnumItemWidget<'T when 'T: comparison>() =
    inherit ItemList()

    let items = DiscriminatedUnion.asIndexedMap<'T> ()

    override this._Ready() =
        for fileClass in items.Keys() do
            this.AddItem
                (fileClass.ToString()
                 |> NamingConventions.upperCaseSpaceSeparated)

    member this.Select(fileClass: 'T) =
        match items.TryFind fileClass with
        | Some classIndex -> this.Select(classIndex)
        | None -> ()
