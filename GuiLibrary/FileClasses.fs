namespace GuiLibrary

open Godot
open Fold
open Utilities

type FileClassesFs() as this =
    inherit ItemList()

    let fileClasses =
        DiscriminatedUnion.allCases<FileClass> ()
        |> Seq.mapi (fun i e -> e, i)
        |> Seq.toList
        |> Map.ofList

    override this._Ready() =
        for fileClass in fileClasses.Keys() do
            this.AddItem
                (fileClass.ToString()
                 |> NamingConventions.upperCaseSpaceSeparated)

    member this.Select(fileClass: FileClass) =
        match fileClasses.TryFind fileClass with
            | Some classIndex -> this.Select(classIndex)
            | None -> ()
