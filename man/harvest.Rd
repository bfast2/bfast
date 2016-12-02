\name{harvest}
\alias{harvest}
\docType{data}
\title{
16-day NDVI time series for a Pinus radiata plantation.
}
\description{
A univariate time series object of class "ts". Frequency is set to 23 -- the approximate number of observations per year.
}

\usage{data(harvest)}

\source{
Verbesselt, J., R. Hyndman, G. Newnham, and D. Culvenor (2009). 
Detecting trend and seasonal changes in satellite image time series. 
\emph{Remote Sensing of Environment}. \url{http://dx.doi.org/10.1016/j.rse.2009.08.014}. 
Or see \url{http://robjhyndman.com/papers/bfast1}.
}

\examples{
plot(harvest,ylab='NDVI')
# References
citation("bfast")
}
\keyword{datasets}
\keyword{ts}
