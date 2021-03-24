namespace GuiLibrary

open Godot
open Fold


type MainPanelFs() =
    inherit PanelContainer()

    member this.Fold = FoldFile.Empty

    override this._Ready() = GD.Print("Ready")
