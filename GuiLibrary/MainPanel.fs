namespace GuiLibrary

open Godot
open Fold
open Utilities

type MainPanelFs() =
    inherit PanelContainer()

    (* Members *)

    let title = "Origami Editor"

    let mutable Fold = FoldFile.Empty

    let mutable FoldFilePath: string option = None
    let mutable FrameIndex = 0

    let updateFrame update =
        Fold <- FoldFile.updateFrame FrameIndex update Fold


    (* Constructor *)

    override this._Ready() = OS.SetWindowTitle title


    (* Functions *)

    member this.propagateToFields() =
        this.propagateFoldToFields ()
        this.propagateFrameToFields ()

    member this.propagateFoldToFields() =
        let orEmpty = Option.defaultValue ""

        // Single Line Text
        let lineEditItems =
            [ ("Gui Container/Gui Body/File Panel/HBox/Spec/Spec Edit", Fold.spec.ToString())
              ("Gui Container/Gui Body/File Panel/HBox/Creator/Creator Edit", Fold.creator)
              ("Gui Container/Gui Body/File Panel/HBox/Author/Author Edit", Fold.author)
              ("Gui Container/Gui Body/File Panel/HBox/Title/Title Edit", Fold.title) ]

        for (nodePath, updatedValue) in lineEditItems do
            this.GetNode<LineEdit>(new NodePath(nodePath)).Text <- updatedValue

        // Description Updating
        let descriptionPath =
            "Gui Container/Gui Body/File Panel/HBox/Description/Description Edit"

        this.GetNode<TextEdit>(new NodePath(descriptionPath)).Text <- Fold.description

        // Classes
        let classesNode =
            this.GetNode<FileClassesFs>(new NodePath("Gui Container/Gui Body/File Panel/HBox/Classes/Classes List"))

        for fileClass in Fold.classes do
            classesNode.Select fileClass

        // Frames
        let framesNode =
            this.GetNode<ItemList>(new NodePath("Gui Container/Gui Body/File Panel/HBox/Frames/Frames List"))

        let frames =
            Fold.frames
            |> List.mapi (fun i frame -> if frame.title = "" then $"Frame {i}" else frame.title)
            |> (fun frames -> "Key Frame" :: frames)

        framesNode.Clear()

        for (frame) in frames do
            framesNode.AddItem(frame)

        framesNode.Select(0)

    member this.propagateFrameToFields() =
        let orEmpty = Option.defaultValue ""

        let frame =
            if FrameIndex = 0 then
                Fold.keyFrame
            else
                List.tryItem (FrameIndex - 1) (Fold.frames)
                |> Option.defaultValue Frame.Empty


        // Single Line Text
        let lineEditItems =
            [ ("Gui Container/Gui Body/Frame Panel/Hbox/Author/Author Edit", frame.author)
              ("Gui Container/Gui Body/Frame Panel/Hbox/Title/Title Edit", frame.title) ]

        for (nodePath, updatedValue) in lineEditItems do
            this.GetNode<LineEdit>(new NodePath(nodePath)).Text <- updatedValue

        // Description Update
        let descriptionPath =
            "Gui Container/Gui Body/Frame Panel/Hbox/Description/Description Edit"

        this.GetNode<TextEdit>(new NodePath(descriptionPath)).Text <- frame.description

        // Classes
        let classesNode =
            this.GetNode<FrameClassesFs>(new NodePath("Gui Container/Gui Body/Frame Panel/Hbox/Classes/Classes List"))

        for frameClass in frame.classes do
            classesNode.Select frameClass

        // Attributes
        let attributesNode =
            this.GetNode<FrameAttributesFs>
                (new NodePath("Gui Container/Gui Body/Frame Panel/Hbox/Attributes/Attribute List"))

        for frameAttribute in frame.attributes do
            attributesNode.Select frameAttribute

        // Units
        let unitNode =
            this.GetNode<FrameUnitFs>(new NodePath("Gui Container/Gui Body/Frame Panel/Hbox/Unit/Unit List"))

        unitNode.Select(frame.unit)

    (* File Dialog Signals *)
    member this._on_File_Button_CreateNewFile() =
        Fold <- FoldFile.Empty
        FoldFilePath <- None
        this.propagateToFields ()

    member this._on_FileDialog_file_selected(path: string) =
        FoldFilePath <- Some path
        let file = new File()
        file.Open(path, File.ModeFlags.Read) |> ignore
        Fold <- FoldFile.FromJson(file.GetAsText())
        file.Close()
        this.propagateToFields ()

    member this._on_FileDialogSave_file_selected(path: string) =
        FoldFilePath <- Some path
        let file = new File()
        file.Open(path, File.ModeFlags.Write) |> ignore
        file.StoreString(FoldFile.ToJson Fold)
        file.Close()


    (* File Metadata Signals *)

    member this._on_File_Spec_Edit_text_changed(specString: string) =
        match specString with
        | TryParser.Int spec -> (Fold <- FoldFile.setSpec spec Fold)
        | _ -> ()

    member this._on_File_Creator_Edit_text_changed(creator: string) =
        Fold <- FoldFile.setCreator creator Fold

    member this._on_File_Author_Edit_text_changed(author: string) =
        Fold <- FoldFile.setAuthor author Fold

    member this._on_File_Title_Edit_text_changed(title: string) =
        Fold <- FoldFile.setTitle title Fold

    member this._on_File_Description_Edit_text_changed() =
        let descriptionPath =
            "Gui Container/Gui Body/File Panel/HBox/Description/Description Edit"

        let descriptionNode =
            this.GetNode<LineEdit>(new NodePath(descriptionPath))

        Fold <- FoldFile.setDescription descriptionNode.Text Fold

    member this._on_File_Classes_List_item_selected(index: int) =
        let selected =
            DiscriminatedUnion.fromIndex<FileClass> index

        Fold <- FoldFile.addClass selected Fold

    member this._on_File_Frames_List_item_selected(index: int) =
        let numFrames = 1 + List.length Fold.frames

        // Update current frame
        if index < numFrames then
            FrameIndex <- index
            this.propagateToFields ()

        // Create new frame
        else
            let newFrames = List.append Fold.frames [ Frame.Empty ]
            Fold <- FoldFile.setFrames newFrames Fold

    (* File Metadata Signals *)
    member this._on_Frame_Author_Edit_text_changed(author: string) =
        updateFrame (Frame.setAuthor author)

    member this._on_Frame_Title_Edit_text_changed(title: string) =
        updateFrame (Frame.setTitle title)

    member this._on_Frame_Frame_Description_Edit_text_changed() =
        let descriptionNode =
            this.GetNode<TextEdit>(new NodePath("Gui Container/Gui Body/Frame Panel/Hbox/Description/Description Edit"))

        updateFrame (Frame.setDescription descriptionNode.Text)

    member this._on_Frame_Classes_List_item_selected(index: int) = failwith "Unimplemented"

    member this._on_Frame_Attribute_List_item_selected(index: int) = failwith "Unimplemented"


    member this._on_Frame_Unit_List_item_selected(index: int) =
        let unit = DiscriminatedUnion.fromIndex index
        updateFrame (Frame.setUnit unit)
