
devtools::install("sm")

library(sm)
library(tidyverse)

n <- 100
x <- runif(n)
y <- rnorm(n)
d <- data.frame(x, y)
ggplot(d, aes(x, y)) + geom_point() + geom_smooth()
est <- sm.regression(x, y, col = '#619CFF', se = TRUE)
sm.regression(x, y, se = TRUE, style = 'standard')
sm.regression(x, y, model = 'no effect', style = 'standard')
est <- sm.regression(x, y, model = 'no effect')
est$plot + ggtitle('Something')

# est <- data.frame(x = est$eval.points, y = est$estimate)
# ggplot(d, aes(x, y)) + geom_point() + geom_line(data = est)
