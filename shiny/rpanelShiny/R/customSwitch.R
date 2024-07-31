library(shiny)

customSwitch <- function(inputId, label, state = TRUE, width = "auto", size = "mini", onColor = "success", offColor = "danger") {
  shiny::addResourcePath(
    prefix = "wwwRpanelShiny",
    directoryPath = system.file("www", package = "rpanelShiny")
  )

  shiny::tagList(
    # add javascript/CSS files
    singleton(tags$head(
      tags$script(src="wwwRpanelShiny/bootstrap-switch.min.js"),
      tags$script(src="wwwRpanelShiny/BootstrapSwitchBinding.js"),
      tags$link(rel="stylesheet", type="text/css", href="wwwRpanelShiny/bootstrap-switch.min.css")
    )),
    # create tag and set options as HTML attributes
    tags$input(type = "checkbox", id = inputId, class = "FrissSwitch",
               "data-state" = tolower(state),
               "data-size" = size,
               "data-label-width" = width,
               "data-on-color" = onColor,
               "data-off-color" = offColor,
               "data-label-text" = label)
  )
}

