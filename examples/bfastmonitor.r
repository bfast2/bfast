NDVIa <- as.ts(zoo::zoo(som$NDVI.a, som$Time))
plot(NDVIa)
## apply the bfast monitor function on the data
## start of the monitoring period is c(2010, 13)
## and the ROC method is used as a method to automatically identify a stable history
mona <- bfastmonitor(NDVIa, start = c(2010, 13))
mona
plot(mona)
## fitted season-trend model in history period
summary(mona$model)
## OLS-based MOSUM monitoring process
plot(mona$mefp, functional = NULL)
## the pattern in the running mean of residuals
## this illustrates the empirical fluctuation process
## and the significance of the detected break.

NDVIb <- as.ts(zoo(som$NDVI.b, som$Time))
plot(NDVIb)
monb <- bfastmonitor(NDVIb, start = c(2010, 13))
monb
plot(monb)
summary(monb$model)
plot(monb$mefp, functional = NULL)

## set the stable history period manually and use a 4th order harmonic model
bfastmonitor(NDVIb, start = c(2010, 13),
  history = c(2008, 7), order = 4, plot = TRUE)

## just use a 6th order harmonic model without trend
mon <- bfastmonitor(NDVIb, formula = response ~ harmon,
    start = c(2010, 13), order = 6, plot = TRUE)
summary(mon$model)
AIC(mon$model)

## use a custom number of seasonal dummies (11/yr) instead of harmonics
mon <- bfastmonitor(NDVIb, formula = response ~ season,
    start = c(2010, 13), sbins = 11, plot = TRUE)
summary(mon$model)
AIC(mon$model)

\donttest{
## Example for processing raster bricks (satellite image time series of 16-day NDVI images)
f <- system.file("extdata/modisraster.tif", package="bfast")
modisbrick <- terra::rast(f)
data <- unlist(modisbrick[1])
ndvi <- bfastts(data, dates, type = c("16-day"))
plot(ndvi/10000)

## derive median NDVI of a NDVI raster brick
medianNDVI <- terra::app(modisbrick, fun = "median")
terra::plot(medianNDVI)

## helper function to be used with the app() function
xbfastmonitor <- function(x, timestamps = dates) {
	ndvi <- bfastts(x, timestamps, type = c("16-day"))
	ndvi <- window(ndvi, end = c(2011, 14))/10000
	## delete end of the time to obtain a dataset similar to RSE paper (Verbesselt et al.,2012)
	bfm <- bfastmonitor(data = ndvi, start = c(2010, 12), history = c("ROC"))
	return(c(breakpoint = bfm$breakpoint, magnitude = bfm$magnitude))
}

## apply on one pixel for testing
bfm <- bfastmonitor(data = ndvi, start = c(2010, 12), history = c("ROC"))
bfm$magnitude
plot(bfm)
xbfastmonitor(data, dates) ## helper function applied on one pixel

## apply the bfastmonitor function onto a raster brick
timeofbreak <- terra::app(modisbrick, fun=xbfastmonitor)

terra::plot(timeofbreak) ## time of break and magnitude of change
terra::plot(timeofbreak,2) ## magnitude of change
}
