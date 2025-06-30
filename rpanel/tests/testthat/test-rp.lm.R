#     Tests for the rp.lm function

# setwd('rpanel')
# library(devtools)
# library(testthat)
# load_all()
# rp.datalink("~/iCloud/teaching/book/data", "set local directory")

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
cofe_2019 <- rp.wrangle('cofe_2019')
Gpm <- cofe_2019$Giving_per_member
Att <- cofe_2019$Attachment
Imd <- cofe_2019$IMD
test_that('Static mode: transformations with a data argument', {
   expect_no_error(rp.lm(log(Giving_per_member) ~ Attachment + IMD, data = cofe_2019,
                         panel = FALSE, residuals.showing = TRUE)
   )
})
test_that('Static mode: transformations without a data argument', {
   expect_no_error(rp.lm(log(Gpm) ~ Att + Imd, panel = FALSE,
                         residuals.showing = TRUE))
   expect_no_error(rp.lm(Gpm ~ log(Att) + Imd, panel = FALSE,
                         residuals.showing = TRUE))
})

rp.lm(log(Gpm) ~ Att + Imd, panel = FALSE,
      residuals.showing = TRUE)

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

poisons           <- dplyr::mutate(poisons, poison = factor(poison),
                                   treatment = factor(treatment))
poisons$poison    <- factor(paste('p', poisons$poison,    sep = ''))
poisons$treatment <- factor(paste('t', poisons$treatment, sep = ''))

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
test_that('Static mode: shading display', {
   expect_no_error(rp.lm(stime ~ poison, data = poisons, panel = FALSE,
                         comparison.model = ~ 1, uncertainty.display = 'shading'))
})
test_that('Static mode: no display model', {
   expect_no_error(rp.lm(stime ~ poison, data = poisons, panel = FALSE,
                         display.model = NULL))
})
test_that('Static mode: valid display.model', {
   expect_no_error(rp.lm(stime ~ poison, data = poisons, panel = FALSE,
                         display.model = ~ 1))
})
test_that('Static mode: invalid display.model', {
   expect_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE,
                      display.model = ~ something))
})
test_that('Static mode: missing data present', {
   poisons1 <- poisons
   poisons1[cbind(sample(1:nrow(poisons1), 8), sample(1:3, 8, replace = TRUE))] <- NA
   expect_no_error(rp.lm(stime ~ poison, data = poisons1, panel = FALSE))
   expect_no_error(rp.lm(stime ~ poison, data = poisons1, panel = FALSE,
                         comparison.model = ~ 1))
})
test_that('Static mode: some categories with no data', {
   ind      <- which((poisons$poison ==  'p1'))
   poisons1 <- poisons[-ind, ]
   expect_no_error(rp.lm(stime ~ poison, data = poisons1, panel = FALSE,
                         comparison.model = ~ 1))
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
test_that('Static mode: missing data present', {
   poisons1 <- poisons
   poisons1[cbind(sample(1:nrow(poisons1), 8), sample(1:3, 8, replace = TRUE))] <- NA
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons1, panel = FALSE))
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons1, panel = FALSE,
                         comparison.model = ~ poison * treatment))
})
test_that('Static mode: some categories with no data', {
   ind      <- which((poisons$poison ==  'p1') & (poisons$treatment == 't3'))
   poisons1 <- poisons[-ind, ]
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons1, panel = FALSE,
                         comparison.model = ~ poison * treatment))
})

#----------------------------------------------------------------
cat('\nPlot model nodes\n')
#----------------------------------------------------------------

path <- rp.datalink("DO_Clyde")
load(path)
clyde.sub  <- subset(clyde, Station == 4)

test_that('Static mode: plot nodes - one highlight', {
   expect_no_error(rp.lm(DO ~ Temperature + Salinity, data = clyde.sub,
                         panel = FALSE, plot.nodes = TRUE))
})
test_that('Static mode: plot nodes - comparison', {
   expect_no_error(rp.lm(DO ~ Temperature + Salinity, data = clyde.sub,
                         comparison.model = ~ Temperature,
                         panel = FALSE, plot.nodes = TRUE))
})
test_that('Static mode: plot nodes - comparison', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons,
                         comparison.model = ~ poison * treatment,
                         panel = FALSE, plot.nodes = TRUE))
})

#----------------------------------------------------------------
cat('\nSave and amend the ggplot object\n')
#----------------------------------------------------------------

test_that('Static mode: plot if no assignment', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons,
                                comparison.model = ~ poison * treatment,
                                panel = FALSE))
   # A break in the sequence of plots for ease of review
   plot(4)
})
test_that('Static mode: no plot if there is an assignment', {
   expect_no_error(plt <- rp.lm(stime ~ poison + treatment, data = poisons,
                                comparison.model = ~ poison * treatment,
                                panel = FALSE))
   plot(5)
   print(plt)
   print(plt + ggplot2::ggtitle("Something"))
})
