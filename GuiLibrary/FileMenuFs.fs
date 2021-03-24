namespace GuiLibrary

open Godot

type FileMenuFs() =
    inherit MenuButton()


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


    member this.Open() = GD.Print("Open New")
    member this.SaveAs() = GD.Print("Save New")
    member this.Quit() = this.GetTree().Quit()
