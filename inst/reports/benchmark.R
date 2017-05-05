library(zoo)
library(bfast)
library(strucchange)
NDVIa <- as.ts(zoo(som$NDVI.a, som$Time))

# history = ROC
bfastmonitor.hist.roc <- function() {
  replicate(300,bfastmonitor(NDVIa, start = c(2010, 13),history = "ROC", type="OLS-MOSUM",formula = response ~ trend + harmon,order=3,lag = NULL, slag = NULL))
}


bfastmonitor.olscusum <- function() {
  replicate(300,bfastmonitor(NDVIa, start = c(2010, 13),history = "ROC", type="OLS-CUSUM",formula = response ~ trend + harmon,order=3,lag = NULL, slag = NULL))
}

bfastmonitor.RE <- function() {
  replicate(50,bfastmonitor(NDVIa, start = c(2010, 13),history = "ROC", type="RE",formula = response ~ trend + harmon,order=3,lag = NULL, slag = NULL))
}

bfastmonitor.ME <- function() {
  replicate(50,bfastmonitor(NDVIa, start = c(2010, 13),history = "ROC", type="ME",formula = response ~ trend + harmon,order=3,lag = NULL, slag = NULL))
}

# history = BP
bfastmonitor.hist.bp <- function() {
  replicate(5,bfastmonitor(NDVIa, start = c(2010, 13),history = "BP", type="OLS-MOSUM",formula = response ~ trend + harmon,order=3,lag = NULL, slag = NULL))
}


# history = all
bfastmonitor.hist.all <- function() {
  replicate(300,bfastmonitor(NDVIa, start = c(2010, 13),history = "all", type="OLS-MOSUM",formula = response ~ trend + harmon,order=3,lag = NULL, slag = NULL))
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
  x <-replicate(10,calc(modisbrick,function(x) {xbfastmonitor(x, dates)}))
}

bfastmonitor.modis.all <- function() {
  ## helper function to be used with the calc() function
  xbfastmonitor <- function(x,dates) {
    ndvi <- bfastts(x, dates, type = c("16-day"))
    ndvi <- window(ndvi,end=c(2011,14))/10000
    ## delete end of the time to obtain a dataset similar to RSE paper (Verbesselt et al.,2012)
    bfm <- bfastmonitor(data = ndvi, start=c(2010,12), history = c("all"))
    return(cbind(bfm$breakpoint, bfm$magnitude))
  }
  x <-replicate(10,calc(modisbrick,function(x) {xbfastmonitor(x, dates)}))
}


bfastmonitor.modis.all.irregular <- function() {
  ## helper function to be used with the calc() function
  xbfastmonitor <- function(x,dates) {
    ndvi <- bfastts(x, dates, type = c("irregular"))
    ndvi <- window(ndvi,end=c(2011,14))/10000
    ## delete end of the time to obtain a dataset similar to RSE paper (Verbesselt et al.,2012)
    bfm <- bfastmonitor(data = ndvi, start=c(2010,12), history = c("all"))
    return(cbind(bfm$breakpoint, bfm$magnitude))
  }
  x <-replicate(10,calc(modisbrick,function(x) {xbfastmonitor(x, dates)}))
}




rdist <- 10/length(harvest)
require(forecast)

bfast.reccusum <- function() {
  replicate(5,bfast(harvest,h=rdist, type="Rec-CUSUM", season="harmonic", max.iter=1))
}

bfast.olscusum <- function() {
  replicate(5,bfast(harvest,h=rdist, type="OLS-CUSUM", season="harmonic", max.iter = 1))
}


bfast.dummy <- function() {
  replicate(5,bfast(harvest,h=rdist, type="OLS-CUSUM", season="dummy", max.iter = 1))
}

bfast.olsmosum <- function() {
  replicate(5,bfast(harvest,h=rdist, type="OLS-MOSUM", season="harmonic", max.iter = 1))
}

bfast.recmosum <- function() {
  replicate(5,bfast(harvest,h=rdist, type="Rec-MOSUM", season="harmonic", max.iter = 1))
}



bfast01.test01 <- function() {
  replicate(50, bfast01(NDVIa))
}

bfast01.test02 <- function() {
  replicate(50, bfast01(NDVIa, test = c("BIC", "OLS-MOSUM", "supLM"), aggregate = any))
}

bfast01.test03 <- function() {
  replicate(50, bfast01(NDVIa, test = c("OLS-MOSUM", "supLM"), aggregate = any, bandwidth = 0.11)) 
}


