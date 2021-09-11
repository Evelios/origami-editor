namespace Gui.Widgets

module Dialogs =
    open System
    open Avalonia.Controls

    (* Open up a file dialog for selecting files of a particular type. *)
    let openFileDialogTask (title: string) (extensions: string seq) (window: Window) : Unit -> Async<string array> =
        let dialog = OpenFileDialog()

        let filters =
            let filter = FileDialogFilter()
            filter.Extensions <- Collections.Generic.List(extensions)
            filter.Name <- title
            Collections.Generic.List(seq { filter })

        dialog.Filters <- filters
        dialog.AllowMultiple <- false
        dialog.Title <- $"Open {title}"

        fun () -> dialog.ShowAsync(window) |> Async.AwaitTask

    (* Open up a file dialog for saving a file type. *)
    let saveFileDialogTask (title: string) (extensions: string seq) (window: Window) : Unit -> Async<string> =
        let dialog = SaveFileDialog()

        let filters =
            let filter = FileDialogFilter()
            filter.Extensions <- Collections.Generic.List(extensions)
            filter.Name <- title
            Collections.Generic.List(seq { filter })

        dialog.InitialFileName <- "Fold File.fold"
        dialog.Filters <- filters
        dialog.Title <- $"Save {title}"

        fun () -> dialog.ShowAsync(window) |> Async.AwaitTask
