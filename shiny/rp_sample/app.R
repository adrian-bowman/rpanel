library(shiny)

sample.plot <- function(panel) {
   mu    <- panel$mean
   sigma <- panel$sigma
   n     <- panel$n
   col.pars <- "green3"
   with(panel, {
      par(mfrow = c(2, 1), mar = c(2, 1.5, 1, 1) + 0.1, mgp = c(1, 0.2, 0), tcl = -0.2,
          yaxs = "i")
      hst <- hist(y, plot = FALSE,
                  breaks = seq(min(y, mu - 3 * sigma), max(y, mu + 3 * sigma), length = 20))
      plot(mu + c(-3, 3) * sigma, c(0, 1 / sigma),
           type = "n", axes = FALSE, xlab = "Data", ylab = "")
      usr <- par("usr")
      rect(usr[1], usr[3], usr[2], usr[4], col = grey(0.9), border = NA)
      grid(col = "white", lty = 1)
      axis(1, lwd = 0, lwd.ticks = 2, col = grey(0.6), col.ticks = grey(0.6),
           col.axis = grey(0.6), cex.axis = 0.8)
      # axis(2, lwd = 0, lwd.ticks = 2, col = grey(0.6), col.ticks = grey(0.9),
      # col.axis = grey(0.6), cex.axis = 0.8)
      if ("data" %in% display.sample) {
         hist(y, probability = TRUE, add = TRUE,
              col = grey(0.5), border = grey(0.9),
              breaks = seq(min(y, mu - 3 * sigma), max(y, mu + 3 * sigma), length = 20),
              axes = FALSE, ylab = "", main = "")
         ind <- which(hst$density > usr[4])
         if (length(ind) > 0)
            segments(hst$breaks[ind], usr[4], hst$breaks[ind + 1], usr[4], col = "red", lwd = 3)
         ind <- any(hst$density > 0 & hst$breaks[-1] < usr[1])
         if (ind) segments(usr[1], 0, usr[1], usr[4], col = "red", lwd = 3)
         ind <- any(hst$density > 0 & hst$breaks[-length(hst$breaks)] > usr[2])
         if (ind) segments(usr[2], 0, usr[2], usr[4], col = "red", lwd = 3)
      }
      if ("population" %in% display.sample) {
         xgrid <- seq(usr[1], usr[2], length = 100)
         lines(xgrid, dnorm(xgrid, mu, sigma), col = "blue", lwd = 2)
      }
      if ("mean" %in% display.sample)
         lines(rep(mu, 2), c(0, 0.9 / sigma), col = col.pars, lwd = 2)
      if ("+/- 2 st.dev." %in% display.sample)
         arrows(mu - 2 * sigma, 0.8 / sigma,
                mu + 2 * sigma, 0.8 / sigma,
                length = 0.05, code = 3, col = col.pars, lwd = 2)
      if (length(display.mean) > 0) {
         hst <- hist(mns, plot = FALSE,
                     breaks = seq(mu - 3 * sigma, mu + 3 * sigma, length = 50))
         plot(mu + c(-3, 3) * sigma, c(0, sqrt(n) / sigma),
              type = "n", axes = FALSE, xlab = "Sample mean", ylab = "")
         usr <- par("usr")
         rect(usr[1], usr[3], usr[2], usr[4], col = grey(0.9), border = NA)
         grid(col = "white", lty = 1)
         axis(1, lwd = 0, lwd.ticks = 2, col = grey(0.6), col.ticks = grey(0.6),
              col.axis = grey(0.6), cex.axis = 0.8)
         if ("sample mean" %in% display.mean) {
            hist(mns, probability = TRUE, add = TRUE,
                 breaks = seq(mu - 3 * sigma, mu + 3 * sigma, length = 50),
                 axes = FALSE, col = grey(0.5), border = grey(0.9), ylab = "", main = "")
         }
         if ("mean" %in% display.sample)
            lines(rep(mu, 2), c(0, 0.9 * usr[4]), col = col.pars, lwd = 2)
         if ("+/- 2 se" %in% display.mean)
            arrows(mu - 2 * sigma / sqrt(n), 0.8 * usr[4],
                   mu + 2 * sigma / sqrt(n), 0.8 * usr[4],
                   length = 0.05, code = 3, col = col.pars, lwd = 2)
         if ("distribution" %in% display.mean) {
            xgrid <- seq(usr[1], usr[2], length = 200)
            lines(xgrid, dnorm(xgrid, mu, sigma / sqrt(n)), col = "blue", lwd = 2)
         }
      }
      par(mfrow = c(1, 1))         
   })
   panel
}

sample.new <- function(panel) {
   panel$y <- rnorm(panel$n, panel$mean, panel$sigma)
   if (!("accumulate" %in% panel$display.mean)) panel$mns <- NULL
   panel$mns <- c(mean(panel$y), panel$mns)
   panel
}

ui <- fluidPage(
   titlePanel("Sampling"),
   sidebarLayout(      
      sidebarPanel(
         numericInput("mean",  "mean", 0),
         numericInput("sigma", "st.dev.",   1),
         numericInput("n", "sample size",   25),
         actionButton("sample", "Sample"),
         checkboxGroupInput("display.sample", "Sample",
                            c("data", "population", "mean", "+/- 2 st.dev."),
                            "data"),
         checkboxGroupInput("display.mean", "Sample mean",
                            c("sample mean", "accumulate", "+/- 2 se", "distribution"))
      ),
      mainPanel(
         plotOutput("plot")  
      )
   )
)

server <- function(input, output) {
  
   vars <- reactiveValues(mns = NULL)

   observeEvent({input$n | input$mean | input$sigma}, {
      vars$mns <- NULL
   })
   
   observeEvent(input$sample, {
      panel       <- list(mean = input$mean, sigma = input$sigma,
                          n = input$n, display.sample = input$display.sample,
                          display.mean = input$display.mean, mns = vars$mns)
      # panel       <- sample.new(panel)
      panel$y     <- rnorm(panel$n, panel$mean, panel$sigma)
      mn          <- mean(panel$y)
      panel$mns   <- if ("accumulate" %in% panel$display.mean) c(mn, panel$mns) else mn
      vars$mns  <- panel$mns
      output$plot <- renderPlot(sample.plot(panel))
   })
}

shinyApp(ui, server)
