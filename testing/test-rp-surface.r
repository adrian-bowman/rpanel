#     Examples of rp.surface

library(rpanel)
if (reinstall) devtools::install("rpanel")

library(sm)
with(trawl, {
location  <- cbind(Longitude, Latitude)
model   <- sm.regression(location, Score1, ngrid = 15, display = "none")
longitude <- model$eval.points[ , 1]
latitude  <- model$eval.points[ , 2]
xgrid     <- as.matrix(expand.grid(longitude, latitude))
S         <- sm.weight2(location, xgrid, model$h)
covar     <- tcrossprod(S) * model$sigma^2

rp.surface(model$estimate, covar, longitude, latitude,
   location, Score1)
})
