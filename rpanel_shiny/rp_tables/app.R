library(rpanel)
library(shiny)

ui <- fluidPage(
   titlePanel("Distributions"),
   sidebarLayout(      
      sidebarPanel(
         radioButtons("distribution", "Distribution",
                            c("normal", "t", "chi-squared", "F"), "normal"),
         numericInput("degf1",  "df1",  5),
         numericInput("degf2",  "df2", 30),
         numericInput("observed.value", "Observed value", NA),
         radioButtons("tail.probability", "Tail probability",
                            c("none", "from observed value", "fixed probability"), "none"),
         numericInput("probability", "Fixed probability", 0.05),
         radioButtons("tail.direction", "Tail direction",
                            c("lower", "upper", "two-sided"), "two-sided")
      ),
      mainPanel(
         plotOutput("density")  
      )
   )
)

server <- function(input, output) {
  
   observeEvent(input$distribution, {
      # output$density <- renderPlot(tables.draw(input))
      output$density <- renderPlot(
                           rp.tables(panel = FALSE,
                                     distribution = input$distribution,
                                     degf1 = input$degf1,
                                     degf2 = input$degf2,
                                     observed.value = input$observed.value,
                                     observed.value.showing = !is.na(input$observed.value),
                                     probability = input$probability,
                                     tail.probability = input$tail.probability,
                                     tail.direction = input$tail.direction)
                           )
   })
}

shinyApp(ui, server)
