

#' Breaks For Additive Season and Trend (BFAST)
#' 
#' @description
#' BFAST integrates the decomposition of time series into trend, seasonal, and
#' remainder components with methods for detecting and characterizing abrupt
#' changes within the trend and seasonal components. BFAST can be used to
#' analyze different types of satellite image time series and can be applied to
#' other disciplines dealing with seasonal or non-seasonal time series,such as
#' hydrology, climatology, and econometrics. The algorithm can be extended to
#' label detected changes with information on the parameters of the fitted
#' piecewise linear models.
#' 
#' Additionally monitoring disturbances in BFAST-type models at the end of time
#' series (i.e., in near real-time) is available: Based on a model for stable
#' historical behaviour abnormal changes within newly acquired data can be
#' detected. Different models are available for modeling the stable historical
#' behavior. A season-trend model (with harmonic seasonal pattern) is used as a
#' default in the regresssion modelling.
#' 
#' @details
#' The package contains:
#' 
#' * [bfast()]: Main function for iterative decomposition and break detection as described in
#' Verbesselt et al (2010a,b).
#' * [bfastlite()]: lightweight and fast detection of all breaks in a time series
#' using a single iteration with all components at once.
#' * [bfastmonitor()]:
#' Monitoring approach for detecting disturbances in near real-time (see
#' Verbesselt et al. 2012).
#' * [bfastpp()]: Data pre-processing for BFAST-type modeling.
#' * Functions for plotting and printing, see [bfast()].
#' * [simts]: Artificial example data set.
#' * [harvest]: NDVI time series of a P. radiata plantation
#' that is harvested.
#' * [som]: NDVI time series of
#' locations in the south of Somalia to illustrate the near real-time
#' disturbance approach
#' 
#' @section Package options: bfast uses the following options to modify the
#' default behaviour:
#' * `bfast.prefer_matrix_methods`:
#' logical value defining whether methods should try to
#' use the design matrix instead of the formula and a dataframe whenever
#' possible. This can avoid expensive repeated calls of \code{model.matrix} and
#' \code{model.frame} and make model fitting faster using \code{lm.fit}.
#' * `bfast.use_bfastts_modifications`:
#' logical value defining whether a faster version of [bfastts()] should be used.
#' * `strucchange.use_armadillo`:
#' logical value defining whether to use C++ optimised code paths in strucchangeRcpp.
#' 
#' By default, all three are enabled.
#' See [set_fallback_options()] for a convenient interface for setting them all off
#' for debugging purposes.
#' @name bfast-package
#' @references \insertRef{janbfastmonitor}{bfast}
#'
#' \insertRef{janbfast}{bfast}
#'
#' \insertRef{janbfastpheno}{bfast}
#' @keywords ts
#' @import strucchangeRcpp zoo forecast stats
#' @importFrom Rcpp evalCpp
#' @importFrom Rdpack reprompt
#' @importFrom graphics abline axis box layout legend lines mtext par plot points rect text arrows
#' @importFrom utils head tail
#' @useDynLib bfast
NULL





#' A vector with date information (a Datum type) to be linked with each NDVI
#' layer within the modis raster brick (modisraster data set)
#' 
#' \code{dates} is an object of class "Date" and contains the "Date"
#' information to create a 16-day time series object.
#' 
#' 
#' @name dates
#' @docType data
#' @source \insertRef{janbfastmonitor}{bfast}
#' @keywords datasets
#' @examples
#' 
#' ## see ?bfastmonitor for examples
#' 
NULL





#' 16-day NDVI time series for a Pinus radiata plantation.
#' 
#' A univariate time series object of class "ts". Frequency is set to 23 -- the
#' approximate number of observations per year.
#' 
#' 
#' @name harvest
#' @docType data
#' @source \insertRef{janbfast}{bfast}
#' @keywords datasets ts
#' @examples
#' 
#' plot(harvest,ylab='NDVI')
#' # References
#' citation("bfast")
#' 
NULL





#' A raster brick of 16-day satellite image NDVI time series for a small subset
#' in south eastern Somalia.
#' 
#' A raster brick containing 16-day NDVI satellite images (MOD13C1 product).
#' 
#' 
#' @name modisraster
#' @docType data
#' @source \insertRef{janbfastmonitor}{bfast}
#' @keywords datasets ts
#' @examples
#' 
#' 
#' ## see ?bfastmonitor
#' 
#' 
NULL





#' A random NDVI time series
#' 
#' A univariate time series object of class "ts". Frequency is set to 24.
#' 
#' 
#' @name ndvi
#' @docType data
#' @keywords datasets ts
#' @examples
#' 
#' plot(ndvi)
#' 
NULL





#' Simulated seasonal 16-day NDVI time series
#' 
#' \code{simts} is an object of class "stl" and consists of seasonal, trend
#' (equal to 0) and noise components. The simulated noise is typical for
#' remotely sensed satellite data.
#' 
#' 
#' @name simts
#' @docType data
#' @source \insertRef{janbfast}{bfast}
#' @keywords datasets
#' @examples
#' 
#' plot(simts)
#' # References
#' citation("bfast")
#' 
NULL





#' Two 16-day NDVI time series from the south of Somalia
#' 
#' \code{som} is a dataframe containing time and two NDVI time series to
#' illlustrate how the monitoring approach works.
#' 
#' 
#' @name som
#' @docType data
#' @source \insertRef{janbfastmonitor}{bfast}
#' @keywords datasets
#' @examples
#' 
#' ## first define the data as a regular time series (i.e. ts object)
#' library(zoo)
#' NDVI <- as.ts(zoo(som$NDVI.b,som$Time))
#' plot(NDVI)
#' 
#' 
NULL



