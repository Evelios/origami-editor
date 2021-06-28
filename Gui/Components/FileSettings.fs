namespace Gui

open Avalonia.FuncUI.Types
open CreasePattern

module FileSettings =

    open Avalonia.FuncUI.DSL
    open Avalonia.Controls
    open Gui.Widgets

    type Msg =
        | SelectUnit of Fold.LengthUnit
        | ChangeAuthor of string
        | ChangeTitle of string
        | ChangeDescription of string

    let update msg (frame: Frame) : Frame =
        frame
        |> match msg with
           | SelectUnit unit -> Frame.setUnit unit
           | ChangeAuthor author -> Frame.setAuthor author
           | ChangeTitle title -> Frame.setTitle title
           | ChangeDescription description -> Frame.setDescription description


    // View

    let view (frame: Frame) dispatch =
        let settingsOptions : IView list =
            [ TextBlock.create [ TextBlock.text "File Settings" ]

              Form.dropdownSelection
                  {| name = "Units"
                     selected = frame.unit
                     onSelected = Msg.SelectUnit >> dispatch |}

              Form.textItem
                  {| name = "Author"
                     value = frame.author
                     onSelected = Msg.ChangeAuthor >> dispatch |}

              Form.textItem
                  {| name = "Title"
                     value = frame.title
                     onSelected = Msg.ChangeTitle >> dispatch |}

              Form.multiline
                  {| name = "Description"
                     value = frame.description
                     onSelected = Msg.ChangeDescription >> dispatch |} ]

        StackPanel.create
        <| [ StackPanel.width Theme.size.small
             StackPanel.margin Theme.spacing.large
             StackPanel.children settingsOptions ]
