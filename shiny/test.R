library(shiny)

source("R/responsiveSlider.R")

# library(devtools)
# devtools::load_all(".")

ui <- shiny::fluidPage(
  shiny::titlePanel("Density estimate"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      setSliderRatePolicy(
        shiny::sliderInput("bw", "bandwidth", 0.1, 5, 0.1),
        policy="throttle",
        delay=100
      )
    ),
    shiny::mainPanel(
      shiny::plotOutput("plot")
    )
  )
)

server <- function(input, output) {

  output$plot <- renderPlot(density(y, input$bw))

}

y <- rnorm(50)
runApp(shinyApp(ui, server))
