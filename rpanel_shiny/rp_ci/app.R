library(shiny)

   ci.shiny.compute <- function(panel) {
      X     <- matrix(rnorm(panel$n * 100, panel$mean, panel$sigma), ncol = panel$n)
      Xmean <- apply(X, 1, mean)
      Xsd   <- apply(X, 1, sd)
      limit <- qt(1 - (1 - as.numeric(panel$confidence)) / 2, panel$n - 1) * Xsd / sqrt(panel$n)
      panel$lower    <- Xmean - limit
      panel$upper    <- Xmean + limit
      panel$cover    <- (panel$lower < panel$mean) & (panel$mean < panel$upper)
      panel$coverage <- panel$coverage + length(which(panel$cover))
      panel
   }
      
   ci.shiny.draw <- function(panel) {
      plot(panel$mean + c(-5, 5) * panel$sigma / sqrt(30), c(1, 100),
           type = "n", xlab = "y", ylab = "")
      colour <- rep("blue", 100)
      colour[!panel$cover] <- "red"
      segments(panel$lower, 1:100, panel$upper, 1:100, col = colour)
      abline(v = panel$mean, lty = 2)
      title("Simulated confidence intervals", col.main = "red", line = 3, cex.main = 1)
      title(paste("Observed coverage: ", round(100 * panel$coverage / panel$nsim, 2), "%", sep = ""), 
              col.main = "blue", line = 2, cex.main = 1)
      title(paste("(Simulation size: ", panel$nsim, ")", sep = ""), line = 1, cex.main = 1)
      panel
   }
      
ui <- fluidPage(
   titlePanel("Confidence intervals"),
   sidebarLayout(      
      sidebarPanel(
         actionButton("simulate", "Simulate"),
         checkboxInput("accumulate", "Accumulate coverage"),
         numericInput("mean",  "mean", 0),
         numericInput("sigma", "sd",   1),
         selectInput("n", label = h3("Sample size"),
                      choices = list("30" = 30, "50" = 50, "100" = 100), 
                      selected = 30),
         selectInput("confidence", label = h3("Confidence level"),
                      choices = list("0.90" = 0.90, "0.95" = 0.95, "0.99" = 0.99), 
                      selected = 0.95)
      ),
      mainPanel(
         plotOutput("cis")  
      )
   )
)

server <- function(input, output) {
  
   vars <- reactiveValues(count = 0, coverage = 0)

   observeEvent(input$simulate, {
      vars$count <- vars$count + 1
      if (!input$accumulate) {
         vars$count    <- 1
         vars$coverage <- 0
      }
      panel          <- list(mean = as.numeric(input$mean), sigma = as.numeric(input$sigma),
                             n = as.numeric(input$n),
                             confidence = as.numeric(input$confidence),
                             nsim = vars$count * 100, coverage = vars$coverage)
      panel          <- ci.shiny.compute(panel)
      vars$coverage  <- panel$coverage
      panel$coverage <- vars$coverage
      output$cis     <- renderPlot(ci.shiny.draw(panel))
   })
}

shinyApp(ui, server)
