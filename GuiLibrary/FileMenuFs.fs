namespace GuiLibrary

open Godot

type FileMenuFs() as this =
    inherit MenuButton()

    let fileDialog =
        lazy (this.GetNode<FileDialog>(new NodePath("FileDialog")))

    let fileDialogSave =
        lazy (this.GetNode<FileDialog>(new NodePath("FileDialog")))

    member this.MenuItems =
        [ ("Open Fold File", this.Open)
          ("Save As", this.SaveAs)
          ("Quit", this.Quit) ]
        |> Map.ofList

    override this._Ready() =
        for text in this.MenuItems do
            this.GetPopup().AddItem(text.Key)

        this
            .GetPopup()
            .Connect("id_pressed", this, "MenuButtonPressed")
        |> ignore

    member this.MenuButtonPressed id =
        let text = this.GetPopup().GetItemText(id)

        match Map.tryFind text this.MenuItems with
        | Some action -> action ()
        | _ -> failwith $"Could not find menu action for {text}"

    member this.Open() = fileDialog.Value.Popup_()
    member this.SaveAs() = fileDialogSave.Value.Popup_()
    member this.Quit() = this.GetTree().Quit()
