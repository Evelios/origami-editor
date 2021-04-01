namespace GuiLibrary

open Godot

type MetadataNode =
    | FileSpec
    | FileCreator
    | FileAuthor
    | FileTitle
    | FileDescription
    | FileClasses
    | FileFrames
    | FrameAuthor
    | FrameTitle
    | FrameDescription
    | FrameClasses
    | FrameAttributes
    | FrameUnit

type RootNodeFs() as this =
    inherit Node()

    member this.nodeOf node =
        match node with
        (* File *)
        | FileSpec -> "Gui Container/Gui Body/File Panel/HBox/Spec/Spec Edit"
        | FileCreator -> "Gui Container/Gui Body/File Panel/HBox/Creator/Creator Edit"
        | FileAuthor -> "Gui Container/Gui Body/File Panel/HBox/Author/Author Edit"
        | FileTitle -> "Gui Container/Gui Body/File Panel/HBox/Title/Title Edit"
        | FileDescription -> "Gui Container/Gui Body/File Panel/HBox/Description/Description Edit"
        | FileClasses -> "Gui Container/Gui Body/File Panel/HBox/Classes/Classes List"
        | FileFrames -> "Gui Container/Gui Body/File Panel/HBox/Frames/Frames List"
        (* Frame *)
        | FrameAuthor -> "Gui Container/Gui Body/Frame Panel/Hbox/Author/Author Edit"
        | FrameTitle -> "Gui Container/Gui Body/Frame Panel/Hbox/Title/Title Edit"
        | FrameDescription -> "Gui Container/Gui Body/Frame Panel/Hbox/Description/Description Edit"
        | FrameClasses -> "Gui Container/Gui Body/Frame Panel/Hbox/Classes/Classes List"
        | FrameAttributes -> "Gui Container/Gui Body/Frame Panel/Hbox/Attributes/Attribute List"
        | FrameUnit -> "Gui Container/Gui Body/Frame Panel/Hbox/Unit/Unit List"
        |> fun n -> this.GetNode<Node>(new NodePath(n))


module RootNodeFs =
    let get (node: Node): RootNodeFs =
        node.GetNode<RootNodeFs>(new NodePath("/root/RootNode"))
