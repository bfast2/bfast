## Simulated Data
plot(simts) # stl object containing simulated NDVI time series
datats <- ts(rowSums(simts$time.series))
# sum of all the components (season,abrupt,remainder)
tsp(datats) <- tsp(simts$time.series) # assign correct time series attributes
plot(datats)

fit <- bfast(datats, h = 0.15, season = "dummy", max.iter = 1)
plot(fit, sim = simts)
fit
# prints out whether breakpoints are detected
# in the seasonal and trend component
 
## Real data
## The data should be a regular ts() object without NA's
## See Fig. 8 b in reference
plot(harvest, ylab = "NDVI")
# MODIS 16-day cleaned and interpolated NDVI time series

(rdist <- 10/length(harvest))
# ratio of distance between breaks (time steps) and length of the time series

fit <- bfast(harvest, h = rdist, season = "harmonic", max.iter = 1, breaks = 2)
plot(fit)
## plot anova and slope of the trend identified trend segments
plot(fit, ANOVA = TRUE)
## plot the trend component and identify the break with
## the largest magnitude of change
plot(fit, type = "trend", largest = TRUE)

## plot all the different available plots
plot(fit, type = "all")

## output
niter <- length(fit$output) # nr of iterations
out <- fit$output[[niter]]
# output of results of the final fitted seasonal and trend models and
## #nr of breakpoints in both.

## running bfast on yearly data
t <- ts(as.numeric(harvest), frequency = 1, start = 2006)
fit <- bfast(t, h = 0.23, season = "none", max.iter = 1)
plot(fit)
fit

## handling missing values with stlplus
(NDVIa <- as.ts(zoo::zoo(som$NDVI.a, som$Time)))
fit <- bfast(NDVIa, season = "harmonic", max.iter = 1, decomp = "stlplus")
plot(fit)
fit
