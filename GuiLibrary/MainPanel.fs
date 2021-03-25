namespace GuiLibrary

open Godot
open Fold
open Utilities

type MainPanelFs() =
    inherit PanelContainer()


    (* Members *)

    let mutable Fold = FoldFile.Empty


    (* Constructor *)

    override this._Ready() = ()


    (* Functions *)

    member this.propagateFoldToFields() =
        let orEmpty = Option.defaultValue ""
        let or0 = Option.defaultValue 0

        // Single Line Text
        let lineEditItems =
            [ ("Gui Container/Gui Body/File Panel/HBox/Spec/Spec Edit", (Fold.spec |> or0).ToString())
              ("Gui Container/Gui Body/File Panel/HBox/Creator/Creator Edit", Fold.creator |> orEmpty)
              ("Gui Container/Gui Body/File Panel/HBox/Author/Author Edit", Fold.author |> orEmpty)
              ("Gui Container/Gui Body/File Panel/HBox/Title/Title Edit", Fold.title |> orEmpty) ]

        for (nodePath, updatedValue) in lineEditItems do
            this.GetNode<LineEdit>(new NodePath(nodePath)).Text <- updatedValue

        // Description Updating
        let descriptionPath =
            "Gui Container/Gui Body/File Panel/HBox/Description/Description Edit"

        this.GetNode<TextEdit>(new NodePath(descriptionPath)).Text <- (Fold.description |> orEmpty)

        // Classes
        let classesNode =
            this.GetNode<FileClassesFs>(new NodePath("Gui Container/Gui Body/File Panel/HBox/Classes/Classes List"))

        for fileClass in Option.defaultValue [] Fold.classes do
            classesNode.Select fileClass

        // Frames
        let framesNode =
            this.GetNode<ItemList>(new NodePath("Gui Container/Gui Body/File Panel/HBox/Frames/Frames List"))

        let frames =
            Option.defaultValue [] Fold.frames
            |> List.mapi (fun i frame -> Option.defaultValue $"Frame {i + 1}" frame.title)
            |> (fun frames -> "Key Frame" :: frames)

        for (frame) in frames do
            framesNode.AddItem(frame)

        framesNode.Select(0)

    (* Signals *)

    member this._on_FileDialog_file_selected(path: string) =
        let file = new File()
        file.Open(path, File.ModeFlags.Read) |> ignore
        Fold <- FoldFile.FromJson(file.GetAsText())
        this.propagateFoldToFields ()

    member this._on_Spec_Edit_text_changed(specString: string) =
        match specString with
        | TryParser.Int spec -> (Fold <- FoldFile.setSpec (Some spec) Fold)
        | _ -> ()

    member this._on_Creator_Edit_text_changed(creator: string) =
        Fold <- FoldFile.setCreator (Some creator) Fold

    member this._on_Author_Edit_text_changed(author: string) =
        Fold <- FoldFile.setAuthor (Some author) Fold

    member this._on_Title_Edit_text_changed(title: string) =
        Fold <- FoldFile.setTitle (Some title) Fold

    member this._on_Description_Edit_text_changed() = ()
