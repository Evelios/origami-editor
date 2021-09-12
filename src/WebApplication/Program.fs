module Application

open Elmish
open Elmish.React
open Elmish.Navigation
open WebApplication


[<EntryPoint>]
let main _ =
    Program.mkProgram Shell.init Shell.update Shell.view
    |> Program.toNavigable (UrlParser.parseHash Router.pageParser) Shell.setRoute
    |> Program.withReactSynchronous "application"
    |> Program.run

    0
