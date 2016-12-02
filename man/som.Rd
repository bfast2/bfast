\name{som}
\alias{som}
\docType{data}
\title{
Two 16-day NDVI time series from the south of Somalia
}
\description{
\code{som} is a dataframe containing time and two NDVI time series to
illlustrate how the monitoring approach works.
}

\usage{data(som)}

\source{
Needs to be added.
}

\examples{
## first define the data as a regular time series (i.e. ts object)
library(zoo)
NDVI <- as.ts(zoo(som$NDVI.b,som$Time))
plot(NDVI)

}
\keyword{datasets}
