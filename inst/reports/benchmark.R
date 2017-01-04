library(zoo)
library(bfast)
library(strucchange)
NDVIa <- as.ts(zoo(som$NDVI.a, som$Time))

# history = ROC
bfastmonitor.hist.roc <- function() {
  x <- replicate(300,bfastmonitor(NDVIa, start = c(2010, 13),history = "ROC", type="OLS-MOSUM",formula = response ~ trend + harmon,order=3,lag = NULL, slag = NULL), simplify=F)[[1]]
  return(x)
}


bfastmonitor.olscusum <- function() {
  x <- replicate(300,bfastmonitor(NDVIa, start = c(2010, 13),history = "ROC", type="OLS-CUSUM",formula = response ~ trend + harmon,order=3,lag = NULL, slag = NULL), simplify=F)[[1]]
  return(x)
}

bfastmonitor.RE <- function() {
  x <- replicate(50,bfastmonitor(NDVIa, start = c(2010, 13),history = "ROC", type="RE",formula = response ~ trend + harmon,order=3,lag = NULL, slag = NULL), simplify=F)[[1]]
  return(x)
}

bfastmonitor.ME <- function() {
  x <- replicate(50,bfastmonitor(NDVIa, start = c(2010, 13),history = "ROC", type="ME",formula = response ~ trend + harmon,order=3,lag = NULL, slag = NULL), simplify=F)[[1]]
  return(x)
}

# history = BP
bfastmonitor.hist.bp <- function() {
  x <- replicate(5,bfastmonitor(NDVIa, start = c(2010, 13),history = "BP", type="OLS-MOSUM",formula = response ~ trend + harmon,order=3,lag = NULL, slag = NULL), simplify=F)[[1]]
  return(x)
}


# history = all
bfastmonitor.hist.all <- function() {
  x <- replicate(300,bfastmonitor(NDVIa, start = c(2010, 13),history = "all", type="OLS-MOSUM",formula = response ~ trend + harmon,order=3,lag = NULL, slag = NULL), simplify=F)[[1]]
  return(x)
}


modis.raster <- system.file("extdata/modisraster.grd", package="bfast")
require("raster")
modisbrick <- brick(modis.raster)

# from bfastmonitor examples 
bfastmonitor.modis <- function() {
  ## helper function to be used with the calc() function
  xbfastmonitor <- function(x,dates) {
    ndvi <- bfastts(x, dates, type = c("16-day"))
    ndvi <- window(ndvi,end=c(2011,14))/10000
    ## delete end of the time to obtain a dataset similar to RSE paper (Verbesselt et al.,2012)
    bfm <- bfastmonitor(data = ndvi, start=c(2010,12), history = c("ROC"))
    return(cbind(bfm$breakpoint, bfm$magnitude))
  }
  x <-replicate(10,calc(modisbrick,function(x) {xbfastmonitor(x, dates)}), simplify=F)[[1]]
  return(x)
}


rdist <- 10/length(harvest)
require(forecast)

bfast.reccusum <- function() {
  x <- replicate(5,bfast(harvest,h=rdist, type="Rec-CUSUM", season="harmonic", max.iter=1), simplify=F)[[1]]
  return(x)
}

bfast.olscusum <- function() {
  x <- replicate(5,bfast(harvest,h=rdist, type="OLS-CUSUM", season="harmonic", max.iter = 1), simplify=F)[[1]]
  return(x)
}

bfast.olsmosum <- function() {
  x <- replicate(5,bfast(harvest,h=rdist, type="OLS-MOSUM", season="harmonic", max.iter = 1), simplify=F)[[1]]
  return(x)
}

bfast.recmosum <- function() {
  x <- replicate(5,bfast(harvest,h=rdist, type="Rec-MOSUM", season="harmonic", max.iter = 1), simplify=F)[[1]]
  return(x)
}



