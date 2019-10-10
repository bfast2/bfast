# bfast-package
# Copyright (C) 2019  Dainius Masiliūnas, Dongdong Kong
#
# This file is part of Foobar.
#
# Foobar is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# Foobar is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
#
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
#' The package contains: \itemize{ \item \code{\link[bfast]{bfast}}: Main
#' function for iterative decomposition and break detection as described in
#' Verbesselt et al (2010ab). \item \code{\link[bfast]{bfastmonitor}}:
#' Monitoring approach for detecting disturbances in near real-time (see
#' Verbesselt et al. 2011, submitted to Remote Sensing and Environment). \item
#' \code{\link[bfast]{bfastpp}}: Data pre-processing for BFAST-type modeling.
#' \item Functions for plotting and printing, see \code{\link[bfast]{bfast}}.
#' \item \code{\link[bfast]{simts}}: Artificial example data set. \item
#' \code{\link[bfast]{harvest}}: NDVI time series of a P. radiata plantation
#' that is harvested. \item \code{\link[bfast]{som}}: NDVI time series of
#' locations in the south of Somalia to illustrate the near real-time
#' disturbance approach }
#' 
#' @section Package options: bfast uses the following options to modify the
#' default behaviour: \itemize{ \item \code{bfast.prefer_matrix_methods}:
#' logical value (default \code{FALSE}) defining whether methods should try to
#' use the design matrix instead of the formula and a dataframe whenever
#' possible. This can avoid expensive repeated calls of \code{model.matrix} and
#' \code{model.frame} and make model fitting faster using \code{lm.fit}.
#' Setting this option to TRUE requires according functions in the
#' \code{strucchange} package.  }
#' @name bfast-package
#' @author Jan Verbesselt [aut, cre], Achim Zeileis [aut], Rob Hyndman [ctb],
#' Rogier De Jong [ctb]
#' @references Verbesselt J, Zeileis A, Herold M (2012).  Near real-time
#' disturbance detection using satellite image time series.  \emph{Remote
#' Sensing of Environment}, \bold{123}, 98--108.
#' \url{http://dx.doi.org/10.1016/j.rse.2012.02.022}
#' 
#' Verbesselt J, Hyndman R, Newnham G, Culvenor D (2010).  Detecting Trend and
#' Seasonal Changes in Satellite Image Time Series.  \emph{Remote Sensing of
#' Environment}, \bold{114}(1), 106--115.
#' \url{http://dx.doi.org/10.1016/j.rse.2009.08.014}
#' 
#' Verbesselt J, Hyndman R, Zeileis A, Culvenor D (2010).  Phenological Change
#' Detection while Accounting for Abrupt and Gradual Trends in Satellite Image
#' Time Series.  \emph{Remote Sensing of Environment}, \bold{114}(12),
#' 2970--2980.  \url{http://dx.doi.org/10.1016/j.rse.2010.08.003}
#' @keywords ts
#' @import strucchange zoo raster sp forecast
#' @importFrom Rcpp evalCpp
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
#' @source Verbesselt, J., R. Hyndman, G. Newnham, and D. Culvenor (2012).
#' Near real-time disturbance detection using satellite image time series.
#' \emph{Remote Sensing of Environment}.
#' \url{http://eeecon.uibk.ac.at/wopec2/repec/inn/wpaper/2011-18.pdf}.
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
#' @source Verbesselt, J., R. Hyndman, G. Newnham, and D. Culvenor (2009).
#' Detecting trend and seasonal changes in satellite image time series.
#' \emph{Remote Sensing of Environment}.
#' \url{http://dx.doi.org/10.1016/j.rse.2009.08.014}.  Or see
#' \url{http://robjhyndman.com/papers/bfast1}.
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
#' @source Verbesselt, J., R. Hyndman, G. Newnham, and D. Culvenor (2012).
#' Near real-time disturbance detection using satellite image time series.
#' \emph{Remote Sensing of Environment}.
#' \url{http://eeecon.uibk.ac.at/wopec2/repec/inn/wpaper/2011-18.pdf}.
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
#' @source Verbesselt, J., R. Hyndman, G. Newnham, and D. Culvenor (2009).
#' Detecting trend and seasonal changes in satellite image time series.
#' \emph{Remote Sensing of Environment}.
#' \url{http://dx.doi.org/10.1016/j.rse.2009.08.014}.  Or see
#' \url{http://robjhyndman.com/papers/bfast1}.
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
#' @source Needs to be added.
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



