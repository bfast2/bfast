\name{simts}
\alias{simts}
\docType{data}
\title{
Simulated seasonal 16-day NDVI time series
}
\description{
\code{simts} is an object of class "stl" and consists of seasonal, trend (equal to 0) and noise components. The
simulated noise is typical for remotely sensed satellite data.
}

\usage{data(simts)}

\source{
Verbesselt, J., R. Hyndman, G. Newnham, and D. Culvenor (2009). 
Detecting trend and seasonal changes in satellite image time series. 
\emph{Remote Sensing of Environment}. \url{http://dx.doi.org/10.1016/j.rse.2009.08.014}. 
Or see \url{http://robjhyndman.com/papers/bfast1}.
}

\examples{
plot(simts)
# References
citation("bfast")
}
\keyword{datasets}
