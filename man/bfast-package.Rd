\name{bfast-package}
\alias{bfast-package}
\title{Breaks For Additive Season and Trend (BFAST)}

\description{
BFAST integrates the decomposition of time series into trend, seasonal, and remainder
components with methods for detecting and characterizing abrupt changes within the trend and
seasonal components. BFAST can be used to analyze different types of satellite image time
series and can be applied to other disciplines dealing with seasonal or non-seasonal time
series,such as hydrology, climatology, and econometrics. The algorithm can be extended to
label detected changes with information on the parameters of the fitted piecewise linear
models.

Additionally monitoring disturbances in BFAST-type models at the end of time series
(i.e., in near real-time) is available: Based on a model for stable historical behaviour
abnormal changes within newly acquired data can be detected. Different models are available
for modeling the stable historical behavior. A season-trend model (with harmonic seasonal pattern) is
  used as a default in the regresssion modelling.
}

\details{The package contains:
\itemize{
\item \code{\link[bfast]{bfast}}: Main function for iterative decomposition
  and break detection as described in Verbesselt et al (2010ab).
\item \code{\link[bfast]{bfastmonitor}}: Monitoring approach for detecting disturbances in
  near real-time (see Verbesselt et al. 2011, submitted to Remote Sensing and Environment).
\item \code{\link[bfast]{bfastpp}}: Data pre-processing for BFAST-type modeling.
\item Functions for plotting and printing, see  \code{\link[bfast]{bfast}}.
\item \code{\link[bfast]{simts}}: Artificial example data set.
\item \code{\link[bfast]{harvest}}: NDVI time series of a P. radiata plantation that is harvested.
\item \code{\link[bfast]{som}}: NDVI time series of locations in the south of Somalia to
  illustrate the near real-time disturbance approach
}
}

\author{
Jan Verbesselt [aut, cre], Achim Zeileis [aut], Rob Hyndman [ctb], Rogier De Jong [ctb]
}

\references{
Verbesselt J, Zeileis A, Herold M (2012).
  Near real-time disturbance detection using satellite image time series.
  \emph{Remote Sensing of Environment}, \bold{123}, 98--108.
  \url{http://dx.doi.org/10.1016/j.rse.2012.02.022}

Verbesselt J, Hyndman R, Newnham G, Culvenor D (2010).
  Detecting Trend and Seasonal Changes in Satellite Image Time Series.
  \emph{Remote Sensing of Environment}, \bold{114}(1), 106--115.
  \url{http://dx.doi.org/10.1016/j.rse.2009.08.014}

Verbesselt J, Hyndman R, Zeileis A, Culvenor D (2010).
  Phenological Change Detection while Accounting for Abrupt and Gradual Trends in Satellite Image Time Series.
  \emph{Remote Sensing of Environment}, \bold{114}(12), 2970--2980.
  \url{http://dx.doi.org/10.1016/j.rse.2010.08.003}

}

\keyword{ts}
