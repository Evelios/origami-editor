namespace GuiLibrary

open Godot
open Fold
open Utilities

type MainPanelFs() =
    inherit PanelContainer()


    (* Members *)

    let mutable Fold = FoldFile.Empty

    let mutable FoldFilePath: string option = None


    (* Constructor *)

    override this._Ready() = OS.SetWindowTitle "Origami Editor"


    (* Functions *)

    member this.propagateFoldToFields() =
        let orEmpty = Option.defaultValue ""

        // Single Line Text
        let lineEditItems =
            [ ("Gui Container/Gui Body/File Panel/HBox/Spec/Spec Edit", (Fold.spec |> Option.defaultValue 1).ToString())
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

        framesNode.Clear()

        for (frame) in frames do
            framesNode.AddItem(frame)

        framesNode.Select(0)

    (* File Dialog Signals *)
    member this._on_File_Button_CreateNewFile() =
        GD.Print("Handle New File")
        Fold <- FoldFile.Empty
        FoldFilePath <- None
        this.propagateFoldToFields ()

    member this._on_FileDialog_file_selected(path: string) =
        FoldFilePath <- Some path
        let file = new File()
        file.Open(path, File.ModeFlags.Read) |> ignore
        Fold <- FoldFile.FromJson(file.GetAsText())
        file.Close()
        this.propagateFoldToFields ()

    member this._on_FileDialogSave_file_selected(path: string) =
        FoldFilePath <- Some path
        let file = new File()
        file.Open(path, File.ModeFlags.Write) |> ignore
        file.StoreString(FoldFile.ToJson Fold)
        file.Close()


    (* File Metadata Signals *)

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

    member this._on_Description_Edit_text_changed() =
        let descriptionPath =
            "Gui Container/Gui Body/File Panel/HBox/Description/Description Edit"

        let descriptionNode =
            this.GetNode<LineEdit>(new NodePath(descriptionPath))

        Fold <- FoldFile.setDescription (Some descriptionNode.Text) Fold
