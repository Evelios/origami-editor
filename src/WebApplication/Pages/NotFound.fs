module WebApplication.Pages.NotFound

open Feliz

open WebApplication

let view () : PageView =
    { title = "Page Not Found"
      subtitle = "404: Could not locate the page you are looking for"
      body = Html.div [] }
