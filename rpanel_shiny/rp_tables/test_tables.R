input <- list(
   panel = FALSE,
   distribution = "normal",
   degf1 = 5,
   degf2 = 30,
   observed.value.showing = TRUE,
   observed.value = NA,
   probability = 0.05,
   tail.probability = "fixed probability",
   tail.direction = "two.sided",
   missing.heading = TRUE
)

rp.tables(panel = FALSE,
          distribution = input$distribution,
          degf1 = input$degf1,
          degf2 = input$degf2,
          observed.value = input$observed.value,
          observed.value.showing = input$observed.value.showing,
          probability = input$probability,
          tail.probability = input$tail.probability,
          tail.direction = input$tail.direction
)
