# This script defines a slider control where the ratePolicy can be configured.
# This allows controls to update as a slider is moved.

scriptToMakeSlidersResponsive <- function() {
  shiny::addResourcePath(
    prefix = "wwwrpanel",
    directoryPath = system.file("www", package = "rpanel")
  )

  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$script(src = "wwwrpanel/customise-slider.js")
      )
    )
  )
}

setSliderRatePolicy <- function(slider, policy = "debounce", delay = 250) {
  # insert the script into the page by appending a child tag to the slider
  slider <- shiny::tagAppendChild(slider, scriptToMakeSlidersResponsive())

  # activate it by adding properties
  isInputTag <- function(t) {
    if ("name" %in% names(t)) t$name == "input" else FALSE
  }
  inputIndex = which(sapply(slider$children, isInputTag))
  cat("input index", inputIndex, "\n")
  input <- slider$children[[inputIndex]]
  input$attribs$`data-rate-policy` <- policy
  input$attribs$`data-rate-policy-delay` <- delay
  slider$children[[inputIndex]] <- input

  slider
}
