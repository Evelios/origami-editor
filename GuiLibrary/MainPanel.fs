namespace GuiLibrary

open Godot
open Fold

type MainPanelFs() =
    inherit PanelContainer()

    let mutable Fold = FoldFile.Empty

    override this._Ready() = ()

    member this._on_FileDialog_file_selected(path: string) =
        let file = new File()
        file.Open(path, File.ModeFlags.Read) |> ignore
        Fold <- FoldFile.FromJson(file.GetAsText())
