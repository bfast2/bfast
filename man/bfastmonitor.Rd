\name{bfastmonitor}
\alias{bfastmonitor}
\title{Near Real-Time Disturbance Detection Based on BFAST-Type Models}

\description{
  Monitoring disturbances in time series models (with trend/season/regressor
  terms) at the end of time series (i.e., in near real-time). Based on a model
  for stable historical behaviour abnormal changes within newly acquired data
  can be detected. Different models are available for modeling the stable
  historical behavior. A season-trend model (with harmonic seasonal pattern) is
  used as a default in the regresssion modelling.
}

\usage{
bfastmonitor(data, start,
  formula = response ~ trend + harmon, order = 3, lag = NULL, slag = NULL,
  history = c("ROC", "BP", "all"),
  type = "OLS-MOSUM", h = 0.25, end = 10, level = 0.05,
  hpc = "none", verbose = FALSE, plot = FALSE)
}

\arguments{
  \item{data}{A time series of class \code{\link[stats]{ts}}, or another object that
    can be coerced to such. For seasonal components, a frequency greater than 1 is
    required.}
  \item{start}{numeric. The starting date of the monitoring period. Can either be
    given as a float (e.g., \code{2000.5}) or a vector giving period/cycle
    (e.g., \code{c(2000, 7)}).}
  \item{formula}{formula for the regression model. The default is
    \code{response ~ trend + harmon}, i.e., a linear trend and a harmonic season
    component. Other specifications are possible using all terms set up by
    \code{\link[bfast]{bfastpp}}, i.e., \code{season} (seasonal pattern with dummy
    variables), \code{lag} (autoregressive terms), \code{slag} (seasonal autoregressive
    terms), or \code{xreg} (further covariates). See \code{\link[bfast]{bfastpp}}
    for details.}
  \item{order}{numeric. Order of the harmonic term, defaulting to \code{3}.}
  \item{lag}{numeric. Order of the autoregressive term, by default omitted.}
  \item{slag}{numeric. Order of the seasonal autoregressive term, by default omitted.}
  \item{history}{specification of the start of the stable history period. Can either
    be a character, numeric, or a function. If character, then selection is possible
    between reverse-ordered CUSUM (\code{"ROC"}, default), Bai and Perron breakpoint
    estimation (\code{"BP"}), or all available observations (\code{"all"}). If numeric,
    the start date can be specified in the same form as \code{start}. If a function
    is supplied it is called as \code{history(formula, data)} to compute a numeric
    start date.}
  \item{type}{character specifying the type of monitoring process. By default, a MOSUM
    process based on OLS residuals is employed. See \code{\link[strucchange]{mefp}} for
    alternatives.}
  \item{h}{numeric scalar from interval (0,1) specifying the bandwidth relative to the
    sample size in MOSUM/ME monitoring processes.}
  \item{end}{numeric. Maximum time (relative to the history period) that will be monitored
    (in MOSUM/ME processes). Default is 10 times the history period.}
  \item{level}{numeric. Significance level of the monitoring (and ROC, if selected) procedure,
    i.e., probability of type I error.}
  \item{hpc}{character specifying the high performance computing support. Default is
    \code{"none"}, can be set to \code{"foreach"}. See \code{\link[strucchange]{breakpoints}}
    for more details.}
  \item{verbose}{logical. Should information about the monitoring be printed during
    computation?}
  \item{plot}{logical. Should the result be plotted?}
}

\details{
  \code{bfastmonitor} provides monitoring of disturbances (or structural changes) in near
  real-time based on a wide class of time series regression models with optional
  season/trend/autoregressive/covariate terms. See Verbesselt at al. (2011) for details.

  Based on a given time series (typically, but not necessarily, with frequency greater than 1),
  the data is first preprocessed for regression modeling. Trend/season/autoregressive/covariate
  terms are (optionally) computed using \code{\link[bfast]{bfastpp}}. Second, the data
  is split into a history and monitoring period (starting with \code{start}). Third, a subset
  of the history period is determined which is considered to be stable (see also below).
  Fourth, a regression model is fitted to the preprocessed data in the stable history period.
  Fifth, a monitoring procedure is used to determine whether the observations in the monitoring
  period conform with this stable regression model or whether a change is detected.

  The regression model can be specified by the user. The default is to use a linear trend and
  a harmonic season: \code{response ~ trend + harmon}. However, all other terms set up by
  \code{bfastpp} can also be omitted/added, e.g., \code{response ~ 1} (just a constant),
  \code{response ~ season} (seasonal dummies for each period), etc. Further terms precomputed
  by \code{bfastpp} can be \code{lag} (autoregressive terms of specified order), \code{slag}
  (seasonal autoregressive terms of specified order), \code{xreg} (covariates, if \code{data}
  has more than one column).

  For determining the size of the stable history period, various approaches are available.
  First, the user can set a start date based on subject-matter knowledge. Second, data-driven
  methods can be employed. By default, this is a reverse-ordered CUSUM test (ROC). Alternatively,
  breakpoints can be estimated (Bai and Perron method) and only the data after the last
  breakpoint are employed for the stable history. Finally, the user can also supply a function
  for his/her own data-driven method.
}

\value{
  \code{bfastmonitor} returns an object of class \code{"bfastmonitor"}, i.e., a list with components as follows.
  \item{data}{original \code{"ts"} time series,}
  \item{tspp}{preprocessed \code{"data.frame"} for regression modeling,}
  \item{model}{fitted \code{"lm"} model for the stable history period,}
  \item{mefp}{fitted \code{"mefp"} process for the monitoring period,}
  \item{history}{start and end time of history period,}
  \item{monitor}{start and end time of monitoring period,}
  \item{breakpoint}{breakpoint detected (if any).}
  \item{magnitude}{median of the difference between the data and the model prediction in the monitoring period.}
}

\references{
Verbesselt J, Zeileis A, Herold M (2012).
Near real-time disturbance detection using satellite image time series.
\emph{Remote Sensing Of Environment}, \bold{123}, 98--108.
\url{http://dx.doi.org/10.1016/j.rse.2012.02.022}
}

\author{Achim Zeileis, Jan Verbesselt}

\seealso{\code{\link[strucchange]{monitor}}, \code{\link[strucchange]{mefp}}, \code{\link[strucchange]{breakpoints}}}

\examples{

## See Fig. 6 a and b in Verbesselt et al. (2011)
## for more information about the data time series and acknowledgements

library(zoo)
NDVIa <- as.ts(zoo(som$NDVI.a, som$Time))
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

## For more info
?bfastmonitor


## TUTORIAL for processing raster bricks (satellite image time series of 16-day NDVI images)
f <- system.file("extdata/modisraster.grd", package="bfast")
library("raster")
modisbrick <- brick(f)
data <- as.vector(modisbrick[1])
ndvi <- bfastts(data, dates, type = c("16-day"))
plot(ndvi/10000)

## derive median NDVI of a NDVI raster brick
medianNDVI <- calc(modisbrick, fun=function(x) median(x, na.rm = TRUE))
plot(medianNDVI)

## helper function to be used with the calc() function
xbfastmonitor <- function(x,dates) {
	ndvi <- bfastts(x, dates, type = c("16-day"))
	ndvi <- window(ndvi,end=c(2011,14))/10000
	## delete end of the time to obtain a dataset similar to RSE paper (Verbesselt et al.,2012)
	bfm <- bfastmonitor(data = ndvi, start=c(2010,12), history = c("ROC"))
	return(cbind(bfm$breakpoint, bfm$magnitude))
}

## apply on one pixel for testing
ndvi <- bfastts(as.numeric(modisbrick[1])/10000, dates, type = c("16-day"))
plot(ndvi)

bfm <- bfastmonitor(data = ndvi, start=c(2010,12), history = c("ROC"))
bfm$magnitude
plot(bfm)
xbfastmonitor(modisbrick[1], dates) ## helper function applied on one pixel

\dontrun{
## apply the bfastmonitor function onto a raster brick
library(raster)
timeofbreak <- calc(modisbrick, fun=function(x){
  res <- t(apply(x, 1, xbfastmonitor, dates))
	return(res)
})

plot(timeofbreak) ## time of break and magnitude of change
plot(timeofbreak,2) ## magnitude of change

## create a KMZ file and look at the output
KML(timeofbreak, "timeofbreak.kmz")
}

}

\keyword{ts}

