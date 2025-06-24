#     Tests for the rp.lm function

# setwd('rpanel')
# library(devtools)
# library(testthat)
# load_all()
 
#----------------------------------------------------------------
cat('\nRegression with one covariate\n')
#----------------------------------------------------------------

test_that('Standard call', {
   expect_no_error(pnl <- rp.lm(Giving ~ Employ, data = CofE))
   rp.control.dispose(pnl)
})
test_that('Change axis labels', {
   expect_no_error(pnl <- rp.lm(Giving ~ Employ, data = CofE, xlab = 'x', ylab = 'y'))
   rp.control.dispose(pnl)
})
test_that('Model as input', {
   model <- lm(pnl <- Giving ~ Employ, data = CofE)
   expect_no_error(pnl <- rp.lm(model))
   rp.control.dispose(pnl)
})
test_that('Error if no covariate is specified', {
   expect_error(rp.lm(Giving ~ 1, data = CofE))
})

#----------------------------------------------------------------
cat('\nRegression with two covariates\n')
#----------------------------------------------------------------

test_that('Standard call', {
   expect_no_error(pnl <- rp.lm(Giving ~ Employ + Attend, data = CofE))
   rp.control.dispose(pnl)
})
test_that('Change axis labels', {
   expect_no_error(pnl <- rp.lm(Giving ~ Employ + Attend, data = CofE,
                         xlab = 'x', ylab = 'y', zlab = 'z'))
   rp.control.dispose(pnl)
})
test_that('Interaction between two covariates', {
   expect_warning(pnl <- rp.lm(Giving ~ Employ * Attend, data = CofE))
   rp.control.dispose(pnl)
})
test_that('Static mode: change axis labels with a specified model', {
   expect_no_error(rp.lm(Giving ~ Employ + Attend, data = CofE,
                         xlab = 'x', ylab = 'y', zlab = 'z',
                         display.model = ~ Employ, panel = FALSE))
})
test_that('Static mode: residuals showing', {
   expect_no_error(rp.lm(Giving ~ Employ + Attend, data = CofE,
                         xlab = 'x', ylab = 'y', zlab = 'z',
                         residuals.showing = TRUE, panel = FALSE))
})
test_that('Static mode: select the model to be displayed', {
   expect_no_error(rp.lm(Giving ~ Employ + Attend, data = CofE,
                         display.model = ~ Employ, panel = FALSE))
})
test_that('Static mode: select the null model to be displayed', {
   expect_no_error(rp.lm(Giving ~ Employ + Attend, data = CofE,
                         display.model = ~ 1, residuals.showing = TRUE,
                         panel = FALSE))
})

path <- rp.datalink("DO_Clyde")
load(path)
clyde.sub  <- subset(clyde, Station == 4)
test_that('Static mode: plot nodes only', {
   expect_no_error(rp.lm(DO ~ Temperature + Salinity, data = clyde.sub,
                         panel = FALSE, plot.nodes.only = TRUE))
})

# Remove rgl windows
rgl::close3d(rgl::rgl.dev.list())

#----------------------------------------------------------------
      cat('\nOne covariate and one factor\n')
#----------------------------------------------------------------

gullweight <- dplyr::mutate(gullweight, month = factor(month))
test_that('Standard call', {
   expect_no_error(pnl <- rp.lm(weight ~ hab + month, data = gullweight))
   rp.control.dispose(pnl)
})

path <- rp.datalink('rds')
rds  <- read.table(path, header = TRUE, stringsAsFactors = TRUE)
test_that('Static mode: adjust linewidth', {
   expect_no_error(rp.lm(lrate ~ RDS * GA, data = rds, panel = FALSE, linewidth = 2))
   rp.control.dispose(pnl)
})
test_that('Static mode: adjust font sizes', {
   expect_no_error(rp.lm(lrate ~ RDS * GA, data = rds, panel = FALSE, plot = FALSE) +
                      ggplot2::theme(plot.title = ggplot2::element_text(size = 20)) + 
                      ggplot2::theme(axis.text  = ggplot2::element_text(size = 20)) +
                      ggplot2::theme(axis.title = ggplot2::element_text(size = 20)))
})
test_that('Error: character variable', {
   rds$RDS <- as.character(rds$RDS)
   expect_error(rp.lm(lrate ~ RDS * GA, data = rds, panel = FALSE))
   rds$RDS <- factor(rds$RDS)
})

#----------------------------------------------------------------
      cat('\nOne factor\n')
#----------------------------------------------------------------

poisons <- dplyr::mutate(poisons, poison = factor(poison),
                         treatment = factor(treatment))
test_that('Standard call', {
   expect_no_error(pnl <- rp.lm(stime ~ poison, data = poisons))
   rp.control.dispose(pnl)
})
test_that('Static mode: standard call', {
   expect_no_error(rp.lm(stime ~ poison, data = poisons, panel = FALSE))
})
test_that('Static mode: specify display model', {
   expect_no_error(rp.lm(stime ~ poison, data = poisons, panel = FALSE,
                                display.model = ~ poison))
})
test_that('Static mode: specify display and comparison models', {
   expect_no_error(rp.lm(stime ~ poison, data = poisons, panel = FALSE,
                                display.model = ~ poison, comparison.model = ~ 1))
})

#----------------------------------------------------------------
      cat('\nTwo factors\n')
#----------------------------------------------------------------

test_that('Standard call', {
   expect_no_error(pnl <- rp.lm(stime ~ poison + treatment, data = poisons))
   rp.control.dispose(pnl)
})
test_that('Specify a comparison model', {
   expect_no_error(pnl <- rp.lm(stime ~ poison + treatment, data = poisons,
                                comparison.model = ~ poison))
   rp.control.dispose(pnl)
})
test_that('Shading display', {
   expect_no_error(pnl <- rp.lm(stime ~ poison + treatment, data = poisons,
                                uncertainty.display = 'shading'))
   rp.control.dispose(pnl)
})
test_that('Error: display and comparison models are not adjacent', {
   expect_error(rp.lm(stime ~ poison + treatment, data = poisons,
                      display.model = ~ poison * treatment,
                      comparison.model = ~ poison, panel = FALSE))
})
test_that('Static mode: standard call', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE))
})
test_that('Static mode: shading display', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE,
                         uncertainty.display = 'shading'))
})
test_that('Static mode: no display model', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE,
                         display.model = NULL))
})
test_that('Static mode: valid display.model', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE,
                         display.model = ~ poison * treatment))
})
test_that('Static mode: invalid display.model', {
   expect_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE,
                      display.model = ~ something))
})
test_that('Static mode: valid comparison.model', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE,
                         display.model = ~ poison * treatment,
                         comparison.model = ~ poison + treatment))
})
test_that('Static mode: display.model and comparison.model are not adjacent', {
   expect_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE,
                      display = ~ poison, comparison.model = ~ poison * treatment))
})
