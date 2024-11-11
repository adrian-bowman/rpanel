#     rp.twosample tests

library(rpanel)
if (reinstall) devtools::install("rpanel")

test_label("standard call", test.prompt)
library(ggplot2)
x <- rep(0:1, each = 25) + rnorm(50) + 25
y <- as.character(rep(1:2, each = 25))

rp.twosample(x, y)
rp.twosample(x, y, seed = 1234)
rp.twosample(x, y, uncertainty = 'sample mean', seed = 1234)
rp.twosample(x, y, uncertainty = 'reference',   seed = 1234)
rp.twosample(x, y, uncertainty = 'sample mean', scale = TRUE, seed = 1234)

for (uncertainty in c('none', 'sample mean', 'reference')) {
   for (reference in c(0, NA)) {
      for (violin in c(TRUE, FALSE)) {
         for (scale in c(TRUE, FALSE)) {
            cat(uncertainty, reference, violin, '\n')
            rp.twosample(x, y, uncertainty = uncertainty, reference = reference,
                         violin = violin, seed = 1234)
         }
      }
   }
}
rp.twosample(x, y, reference = NULL, seed = 1234)

rp.twosample(x, y, scale = TRUE, seed = 1234)
rp.twosample(x, y, scale = TRUE, seed = 1234) + ggplot2::coord_flip()
rp.twosample(x, y, reference = 1, seed = 1234)
plt <- rp.twosample(x, y)
plt + ggplot2::ggtitle("Two-sample inference")
rp.twosample(x, y, height = 0.5)
rp.twosample(x, y, data_plot = 'jitter')
cat('This one gives a warning for some reason:\n')
rp.twosample(x, y) + ggplot2::xlim(-3, 3)
rp.twosample(x, y, uncertainty = 'g')

rp.sample()
