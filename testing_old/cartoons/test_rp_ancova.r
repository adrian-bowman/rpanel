#     Code to test rp.ancova

library(rpanel)
if (reinstall) devtools::install("rpanel")

devtools::install("rpanel")
library(rpanel)

covariate <- runif(51)
response  <- 2 * covariate + rnorm(51, 0.05)
gp        <- sample(rep(1:3, each = 17))
rp.ancova(covariate, response, gp, model = c(rep(TRUE, 3), FALSE))
rp.ancova(covariate, response, gp, model = c(rep(TRUE, 3), FALSE), ci = FALSE)
rp.ancova(covariate, response, gp, model = rep(TRUE, 4), model0 = c(rep(TRUE, 3), FALSE))

plot(0:1, 0:1, type = 'n', axes = FALSE, xlab = '', ylab = '')
text(0.5, 0.5, 'model')

# The packages "DiagrammeRsvg" and "rsvg" are required to produce a PNG file.
g <- DiagrammeR::create_graph() |>
   DiagrammeR::add_node(label = "y ~ 1", 
            node_aes = DiagrammeR::node_aes(x = 0.5, y = 0.875, fixedsize = FALSE,
                           fontsize = 4,
                           shape = "rectangle", height = 0.02, width = 0.02)) |>
   DiagrammeR::add_node(label = "y ~ covariate", from = 1,
            node_aes = DiagrammeR::node_aes(x = 0.25, y = 0.625, fixedsize = FALSE,
                                            fontsize = 4, shape = "rectangle",
                                            height = 0.02, width = 0.02)) |>
   DiagrammeR::add_node(label = "y ~ gp",        from = 1,
            node_aes = DiagrammeR::node_aes(x = 0.75, y = 0.625, fixedsize = FALSE,
                                            fontsize = 4, shape = "rectangle",
                                            height = 0.02, width = 0.02)) |>
   DiagrammeR::add_node(label = "y ~ covariate + gp", from = 2:3,
            node_aes = DiagrammeR::node_aes(x = 0.5, y = 0.375, fixedsize = FALSE,
                                            fontsize = 4, shape = "rectangle",
                                            height = 0.02, width = 0.02)) |>
   DiagrammeR::add_node(label = "y ~ covariate + gp + covariate:gp", from = 4,
            node_aes = DiagrammeR::node_aes(x = 0.5, y = 0.125, fixedsize = FALSE,
                                            fontsize = 4, shape = "rectangle",
                                            height = 0.02, width = 0.02))
DiagrammeR::export_graph(g, "~/Desktop/temp.png", title = "Models",
                         width = 800, height = 800)



devtools::install("rpanel")
library(rpanel)
data(gullweight)
attach(gullweight)

rp.ancova(hab, weight, month)
rp.ancova(hab, weight, month, style = "new")
rp.ancova(hab, weight, month, style = "old")

model <- lm(weight ~ hab * month, x = TRUE)
summary(model)$coefficients
names(model$coefficients)
plt <- rp.regression(model)

# This doesn't work
# rp.ancova(hab, weight, month, model = c(TRUE, TRUE, TRUE, FALSE),
# model0 = c(TRUE, FALSE, FALSE, TRUE))

# rp.checkbox.change not currently implemented.
# fn <- function(panel) {
#    print(panel$adrian)
#    panel
# }
# panel <- rp.control()
# rp.checkbox(panel, adrian, fn, name = "checkbox")
# rp.checkbox.change(panel, "checkbox", "adrian", TRUE, action = fn)
# rp.checkbox.change(panel, "checkbox", "adrian", FALSE, action = fn)
