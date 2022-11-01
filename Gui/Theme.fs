module Gui.Theme

open Math.Units

let colors =
    {| darkGray = "#202020"
       gray = "#303030"
       lightGray = "#393939"
       lighterGray = "#828282"
       lightOffWhite = "#f0f0f0"
       offWhite = "#e6e6e6"
       yellow = "#FFA123"
       lightYellow = "#E4AE67"
       blue = "#4F2DBE"
       lightBlue = "#D3CAEF"
       green = "#26D81D" |}

let palette =
    {| primary = colors.yellow
       panelBackground = colors.darkGray
       panelAccent = colors.lightGray
       canvasBackdrop = colors.gray
       canvasBackground = colors.offWhite |}

let window =
    {| height = 600.; width = 800. |}


let font =
    {| h1 = 16.; h2 = 14.; normal = 12. |}

let border =
    {| thickness = 1.; cornerRadius = 5. |}

let creasePattern =
    {| maxLength = Length.cssPixels 500. |}

let spacing =
    {| small = 4.
       medium = 8.
       large = 16. |}

let size = {| small = 150. |}
