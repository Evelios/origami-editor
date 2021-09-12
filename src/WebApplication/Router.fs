module WebApplication.Router

open Elmish.UrlParser
open Elmish.Navigation
open Fable.React.Props

type Route =
    | Home
    | Tutorial
    | Examples
    | Download

let pageParser : Parser<Route -> Route, Route> =
    oneOf [ map Home top
            map Tutorial (s "tutorial")
            map Examples (s "examples")
            map Download (s "download") ]

let toHash route =
    match route with
    | Home -> ""
    | Tutorial -> "tutorial"
    | Examples -> "examples"
    | Download -> "download"
    |> (fun r -> sprintf "#/%s" r)

let href = toHash >> Href
let modifyUrl route = route |> toHash |> Navigation.modifyUrl
let newUrl route = route |> toHash |> Navigation.newUrl
