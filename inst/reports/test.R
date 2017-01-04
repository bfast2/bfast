library(zoo)
library(bfast)
library(strucchange)
NDVIa <- as.ts(zoo(som$NDVI.a, som$Time))


# Results of the following functions will be automatically checked for equality using different package versions. Please make sure that the output has
# a simple type that can be compared with isTRUE(all.equal()).

bfastmonitor.hist.roc <- function() {
  x = bfastmonitor(NDVIa, start = c(2010, 13),history = "ROC", type="OLS-MOSUM",formula = response ~ trend + harmon,order=3,lag = NULL, slag = NULL)
  list(x$monitor,
       x$history,
       x$breakpoint,
       x$magnitude)
}



bfastmonitor.olscusum <- function() {
  x = bfastmonitor(NDVIa, start = c(2010, 13),history = "ROC", type="OLS-CUSUM",formula = response ~ trend + harmon,order=3,lag = NULL, slag = NULL)
  list(x$monitor,
       x$history,
       x$breakpoint,
       x$magnitude)
}

bfastmonitor.RE <- function() {
  x = bfastmonitor(NDVIa, start = c(2010, 13),history = "ROC", type="RE",formula = response ~ trend + harmon,order=3,lag = NULL, slag = NULL)
  list(x$monitor,
       x$history,
       x$breakpoint,
       x$magnitude)
}

bfastmonitor.ME <- function() {
  x = bfastmonitor(NDVIa, start = c(2010, 13),history = "ROC", type="ME",formula = response ~ trend + harmon,order=3,lag = NULL, slag = NULL)
  list(x$monitor,
       x$history,
       x$breakpoint,
       x$magnitude)
}


# history = BP
bfastmonitor.hist.bp <- function() {
  x = bfastmonitor(NDVIa, start = c(2010, 13),history = "BP", type="OLS-MOSUM",formula = response ~ trend + harmon,order=3,lag = NULL, slag = NULL)
  list(x$monitor,
       x$history,
       x$breakpoint,
       x$magnitude)
}

# history = all
bfastmonitor.hist.all <- function() {
  x = bfastmonitor(NDVIa, start = c(2010, 13),history = "all", type="OLS-MOSUM",formula = response ~ trend + harmon,order=3,lag = NULL, slag = NULL)
  list(x$monitor,
       x$history,
       x$breakpoint,
       x$magnitude)
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
  calc(modisbrick,function(x) {xbfastmonitor(x, dates)})
}



require(forecast)
rdist <- 10/length(harvest)


bfast.reccusum <- function() {
  x = bfast(harvest,h=rdist, type="Rec-CUSUM", season="harmonic", max.iter=1)
  return(list(x$magnitude, x$Time, x$output[[1]]$ci.Vt, x$output[[1]]$bp.Vt$breakpoints, x$output[[1]]$ci.Vt$confint, x$nobp))
}

bfast.olscusum <- function() {
  x = bfast(harvest,h=rdist, type="OLS-CUSUM", season="harmonic", max.iter = 1)
  return(list(x$magnitude, x$Time, x$output[[1]]$ci.Vt, x$output[[1]]$bp.Vt$breakpoints, x$output[[1]]$ci.Vt$confint, x$nobp))
}

bfast.olsmosum <- function() {
  x = bfast(harvest,h=rdist, type="OLS-MOSUM", season="harmonic", max.iter = 1)
  return(list(x$magnitude, x$Time, x$output[[1]]$ci.Vt, x$output[[1]]$bp.Vt$breakpoints, x$output[[1]]$ci.Vt$confint, x$nobp))
}

bfast.recmosum <- function() {
  x = bfast(harvest,h=rdist, type="Rec-MOSUM", season="harmonic", max.iter = 1)
  return(list(x$magnitude, x$Time, x$output[[1]]$ci.Vt, x$output[[1]]$bp.Vt$breakpoints, x$output[[1]]$ci.Vt$confint, x$nobp))
}

test.recresid <- function() {
	set.seed(1111)
 
	# random data with intercept as the only regressor
	test.interceptonly <- function() {
	  y <- rnorm(300) + rep(c(0, 2), each = 150)
	  x <- matrix(rep(1, length(y),length(y),1))
	  return(recresid.default(x,y, start = ncol(x) + 1, end = nrow(x), tol = sqrt(.Machine$double.eps)/ncol(x)))
	}

	# random data with p regressors
	test.model1 <- function() {
	  n <- 500
	  p <- 4
	  x <- cbind(1,matrix(rnorm(n * p), n, p)) # with intercept!
	  y <- rnorm(n)
	  return(recresid.default(x,y, start = ncol(x) + 1, end = nrow(x), tol = sqrt(.Machine$double.eps)/ncol(x)))
	}

	# generate random dummy variables by sampling from {0,1} and add an intercept
	test.dummy1 <- function() {
	  n <- 300
	  p <- 3
	  x <- matrix(0, n, p)
	  x[,1] = 1
	  x[,2:p] =  sample(c(0,1),size=n*(p-1), replace = T)
	  
	  y <- rnorm(n) + (x %*% c(1:p)) 
	  return(recresid.default(x,y, start = ncol(x) + 1, end = nrow(x), tol = sqrt(.Machine$double.eps)/ncol(x)))
	}

	# multicollinearity: generate a 1 column (intercept) and a 2nd random column, further columns are multiples of the second column
	# this is a worst case scenario, the model will stay rank deficient for all iterations, i.e. recresid
	# will never set check to FALSE and will thus always fit a model in R. Due to some overhead in the computations and calling
	# R from C++, the original R implementation might be slightly faster here.
	test.poor1 <- function() {
	  n <- 300
	  p <- 3 # must be larger than 2
	  x <- matrix(0, n, 2) 
	  x[,1] = 1
	  x[,2] = rnorm(n,5,1)
	  for (i in 3:p) {
		x <- cbind(x,i*x[,2]) # intercept add columns that are multiples of the 2nd column 
	  }
	  y <- rnorm(n) + (x %*% c(1:p)) 
	  return(recresid.default(x,y, start = ncol(x) + 1, end = nrow(x), tol = sqrt(.Machine$double.eps)/ncol(x)))
	}

	return(list(test.interceptonly(),
				test.model1(),
				test.dummy1(),
				test.poor1()))
}



