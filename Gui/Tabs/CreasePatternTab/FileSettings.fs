namespace Gui.Tabs.CreasePatternTab

module FileSettings =

    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types
    open Avalonia.Layout
    
    open CreasePattern
    open Gui.Widgets
    open Gui

    type Msg =
        | SelectUnit of LengthUnit
        | ChangeAuthor of string
        | ChangeTitle of string
        | ChangeDescription of string

    let update msg (creasePattern: CreasePattern) : CreasePattern =
        creasePattern
        |> match msg with
           | SelectUnit unit -> CreasePattern.setUnit unit
           | ChangeAuthor author -> CreasePattern.setAuthor author
           | ChangeTitle title -> CreasePattern.setTitle title
           | ChangeDescription description -> CreasePattern.setDescription description
           

    // View

    let view (creasePattern: CreasePattern) dispatch =
        let settingsOptions : IView list =
            [ TextBlock.create [ TextBlock.text "File Settings" ]

              Form.dropdownSelection
                  {| name = "Units"
                     selected = creasePattern.Unit
                     onSelected = Msg.SelectUnit >> dispatch |}

              Form.textItem
                  {| name = "Author"
                     value = creasePattern.Author
                     onSelected = Msg.ChangeAuthor >> dispatch
                     labelPlacement = Orientation.Vertical
                     |}

              Form.textItem
                  {| name = "Title"
                     value = creasePattern.Title
                     onSelected = Msg.ChangeTitle >> dispatch
                     labelPlacement = Orientation.Vertical
                     |}

              Form.multiline
                  {| name = "Description"
                     value = creasePattern.Description
                     onSelected = Msg.ChangeDescription >> dispatch |} ]

        let panel =
            StackPanel.create
            <| [ StackPanel.width Theme.size.small
                 StackPanel.margin Theme.spacing.large
                 StackPanel.children settingsOptions ]

        Border.create [ Border.borderBrush Theme.palette.panelAccent
                        Border.borderThickness (0., 0., Theme.border.thickness, 0.)
                        Border.child panel ]
