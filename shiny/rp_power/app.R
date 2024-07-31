library(rpanel)
library(shiny)

ui <- fluidPage(
   titlePanel("Power tool"),
   sidebarLayout(      
      sidebarPanel(
         sliderInput("n",     "n",     min = 10,   max = 300, value = 25),
         sliderInput("mu1",   "mu1",   min = -2,   max =   2, value =  0, step = 0.1),
         sliderInput("mu2",   "mu2",   min = -2,   max =   2, value =  1, step = 0.1),
         sliderInput("sigma", "sigma", min =  0.1, max =   5, value =  1),
         checkboxInput("populations.showing", "Show populations")
      ),
      mainPanel(
         plotOutput("power")  
      )
   )
)

server <- function(input, output) {
  
      output$power <- renderPlot(
                        height = function() 400 * (1 + as.numeric(input$populations.showing)),
                        {
                        rp.power(panel = FALSE,
                                 populations.showing = input$populations.showing,
                                 mu1 = input$mu1, mu2 = input$mu2,
                                 sigma = input$sigma, n = input$n)
                        }
                        )
}

shinyApp(ui, server)
